{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Adresse
  ( codePostalPrm
  , rechercherServicesActifs
  , arreterServicesSge
  ) where

import Control.Exception (try, SomeException)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10
  ( initType, initTypeTest, wsRequest, wsRequestTest )
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
  ( ConsulterDonneesTechniquesContractuellesResponseType(..)
  , PointType(..), PointDonneesGeneralesType(..), AdresseInstallationType(..) )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
  ( CodePostalFrancaisType(..), MesureTypeCodeType(..), Chaine15Type(..) )
import qualified Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10 as Arret
import           Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
  (CommanderArretServicesAccesDonneesResponseType)
import qualified Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10 as RSD
import           Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
  ( RechercherServicesAccesDonneesReponseType(..)
  , ServicesSouscritsType(..), ServiceSouscritType(..) )

import Conso.Fr.SiteDB.Orchestration.Types (GetCodePostal)


-- | Retourne le code postal d'un PRM via SGE ConsulterDonneesTechniquesContractuelles.
codePostalPrm :: Bool -> Bool -> GetCodePostal
codePostalPrm verbose prod prm = do
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


-- | Arrête tous les services SGE actifs pour un PRM.
arreterServicesSge :: Bool -> Bool -> Text -> IO [(String, Either (String, String) ())]
arreterServicesSge verbose prod prmT = do
  actifMap <- rechercherServicesActifs verbose prod prmT
  arreterSge verbose prod prmT (Map.elems actifMap)


-- | Recherche les services SGE actifs pour un PRM. Retourne (typeCode → serviceId).
rechercherServicesActifs :: Bool -> Bool -> Text -> IO (Map String String)
rechercherServicesActifs verbose prod prmT = do
  logV verbose $ "SGE RechercherServicesAccesDonnees → PRM " <> T.unpack prmT
  req  <- if prod then RSD.initType prmStr else RSD.initTypeTest prmStr
  resp <- (if prod then RSD.wsRequest else RSD.wsRequestTest) req
            :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
  case resp of
    Left (code, lbl) -> do
      logV verbose $ "SGE RechercherServicesAccesDonnees erreur : " <> code <> " " <> lbl
      return Map.empty
    Right r -> do
      let services = maybe [] servicesSouscritsType_serviceSouscrit
                       (rechercherServicesAccesDonneesReponseType_servicesSouscrits r)
          pairs = mapMaybe toPair services
      logV verbose $ "SGE services actifs : "
        <> show [ (c, e, sid)
                | s <- services
                , let e   = map simpleText15 (serviceSouscritType_etatCode s)
                      sid = simpleText15 (serviceSouscritType_serviceSouscritId s)
                      c   = maybe "?" simpleText (serviceSouscritType_mesuresTypeCode s)
                ]
      return (Map.fromList pairs)
  where
    prmStr = T.unpack prmT
    toPair s =
      let etats = map simpleText15 (serviceSouscritType_etatCode s)
      in if "ACTIF" `notElem` etats then Nothing
         else case serviceSouscritType_mesuresTypeCode s of
           Nothing   -> Nothing
           Just code -> Just ( simpleText code
                             , simpleText15 (serviceSouscritType_serviceSouscritId s) )
    simpleText  (MesureTypeCodeType (Xsd.XsdString s)) = s
    simpleText15 (Chaine15Type (Xsd.XsdString s))      = s


arreterSge :: Bool -> Bool -> Text -> [String] -> IO [(String, Either (String, String) ())]
arreterSge _ _ _ [] = return []
arreterSge verbose prod prmT serviceIds = do
  logV verbose $ "SGE CommanderArretServicesAccesDonnees : " <> show serviceIds
  req  <- mkInit (T.unpack prmT) Arret.SensSOUTIRAGE serviceIds
  resp <- mkWs req :: IO (Either (String, String) CommanderArretServicesAccesDonneesResponseType)
  return $ case resp of
    Left err -> map (, Left err) serviceIds
    Right _  -> map (, Right ()) serviceIds
  where
    mkInit = if prod then Arret.initType else Arret.initTypeTest
    mkWs   = if prod then Arret.wsRequest else Arret.wsRequestTest


logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()
