{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Adresses
  ( verifierAdresses
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException)

import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10
  ( initType, initTypeTest
  , wsRequest, wsRequestTest
  )
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
  ( ConsulterDonneesTechniquesContractuellesResponseType(..)
  , PointType(..)
  , PointDonneesGeneralesType(..)
  , AdresseInstallationType(..)
  )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 (CodePostalFrancaisType(..))
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Gaz.Adict.DonneesTechniques (consulterDonneesTechniques)
import Conso.Fr.Gaz.Adict.Types
  ( RetourDonneesTechniques(..)
  , DonneesTechniques(..)
  , SituationCompteurDetail(..)
  )
import Conso.Fr.Gaz.Adict.Adict (AdictSession)

import Conso.Fr.SiteDB.Orchestration.Types (VerifAdresse(..))


cpSge :: Bool -> Text -> IO (Either String Text)
cpSge prod prm = do
  result <- try $ do
    req <- if prod then initType (T.unpack prm) True
                   else initTypeTest (T.unpack prm) True
    if prod then wsRequest req else wsRequestTest req
  case result of
    Left e -> return $ Left (show (e :: SomeException))
    Right (Left (code, lbl)) -> return $ Left $ code <> " " <> lbl
    Right (Right resp) ->
      let point = consulterDonneesTechniquesContractuellesResponseType_point resp
          dg    = pointType_donneesGenerales point
          addr  = pointDonneesGeneralesType_adresseInstallation dg
      in return $ case adresseInstallationType_codePostal addr of
           Nothing -> Left "code postal absent dans la réponse SGE"
           Just (CodePostalFrancaisType (Xsd.XsdString s)) -> Right (T.pack s)


cpAdict :: AdictSession -> Text -> IO (Either String Text)
cpAdict session pce = do
  result <- consulterDonneesTechniques session pce
  return $ case result of
    Left err -> Left (show err)
    Right rdt ->
      case rdt_donnees rdt >>= dt_situation_compteur >>= scd_code_postal of
        Nothing -> Left "code postal absent dans la réponse ADICT"
        Just cp -> Right cp


verifierAdresses :: Bool -> AdictSession -> Text -> Text -> IO VerifAdresse
verifierAdresses prod session prm pce = do
  eCpPrm <- cpSge prod prm
  eCpPce <- cpAdict session pce
  return $ case (eCpPrm, eCpPce) of
    (Left e, _) -> VerifImpossible $ "SGE: " <> e
    (_, Left e) -> VerifImpossible $ "ADICT: " <> e
    (Right cpP, Right cpC)
      | T.strip cpP == T.strip cpC -> CodePostauxIdentiques
      | otherwise                  -> Mismatch cpP cpC
