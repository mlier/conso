{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Adresses
  ( verifierAdresses
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import System.IO (hPutStrLn, stderr)

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


logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()


cpSge :: Bool -> Bool -> Text -> IO (Either String Text)
cpSge verbose prod prm = do
  logV verbose $ "SGE ConsulterDonneesTechniquesContractuelles → PRM " <> T.unpack prm
  result <- try $ do
    req <- if prod then initType (T.unpack prm) True
                   else initTypeTest (T.unpack prm) True
    if prod then wsRequest req else wsRequestTest req
  case result of
    Left e ->
      let msg = show (e :: SomeException)
      in logV verbose ("SGE exception : " <> msg) >> return (Left msg)
    Right (Left (code, lbl)) ->
      let msg = code <> " " <> lbl
      in logV verbose ("SGE erreur : " <> msg) >> return (Left msg)
    Right (Right resp) ->
      let point = consulterDonneesTechniquesContractuellesResponseType_point resp
          dg    = pointType_donneesGenerales point
          addr  = pointDonneesGeneralesType_adresseInstallation dg
      in case adresseInstallationType_codePostal addr of
           Nothing ->
             logV verbose "SGE : code postal absent dans la réponse"
             >> return (Left "code postal absent dans la réponse SGE")
           Just (CodePostalFrancaisType (Xsd.XsdString s)) ->
             logV verbose ("SGE code postal : " <> s)
             >> return (Right (T.pack s))


cpAdict :: Bool -> AdictSession -> Text -> IO (Either String Text)
cpAdict verbose session pce = do
  logV verbose $ "ADICT DonneesTechniques → PCE " <> T.unpack pce
  result <- consulterDonneesTechniques session pce
  case result of
    Left err ->
      let msg = show err
      in logV verbose ("ADICT erreur : " <> msg) >> return (Left msg)
    Right rdt ->
      case rdt_donnees rdt >>= dt_situation_compteur >>= scd_code_postal of
        Nothing ->
          logV verbose "ADICT : code postal absent dans la réponse"
          >> return (Left "code postal absent dans la réponse ADICT")
        Just cp ->
          logV verbose ("ADICT code postal : " <> T.unpack cp)
          >> return (Right cp)


verifierAdresses :: Bool -> Bool -> AdictSession -> Text -> Text -> IO VerifAdresse
verifierAdresses verbose prod session prm pce = do
  eCpPrm <- cpSge verbose prod prm
  eCpPce <- cpAdict verbose session pce
  return $ case (eCpPrm, eCpPce) of
    (Left e, _) -> VerifImpossible $ "SGE: " <> e
    (_, Left e) -> VerifImpossible $ "ADICT: " <> e
    (Right cpP, Right cpC)
      | T.strip cpP == T.strip cpC -> CodePostauxIdentiques
      | otherwise                  -> Mismatch cpP cpC
