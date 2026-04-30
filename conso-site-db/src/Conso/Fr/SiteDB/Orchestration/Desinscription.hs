{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..)
  , DesinscriptionResult(..)
  , desinscrirePrm
  , desinscrirePce
  , supprimerSite
  ) where

import Control.Monad (forM, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import System.Directory (removeFile)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, SomeException)

import Database.SQLite.Simple (Connection)

import qualified Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10 as Arret
import           Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
  (CommanderArretServicesAccesDonneesResponseType)
import qualified Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10 as RSD
import           Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
  ( RechercherServicesAccesDonneesReponseType(..)
  , ServicesSouscritsType(..)
  , ServiceSouscritType(..)
  )
import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
  ( MesureTypeCodeType(..), Chaine15Type(..) )

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), SiteRef(..))
import Conso.Fr.SiteDB.Storage.Connection (siteDbPath, openSiteDb)
import Conso.Fr.SiteDB.Registry.Operations
  ( lookupBySiteId, unlinkPrm, unlinkPce, deleteFromRegistry )


-- | Callbacks fournis par les librairies extensions pour supprimer leurs données.
data DesinscriptionCallbacks = DesinscriptionCallbacks
  { cbDeleteElec :: Connection -> IO ()  -- ^ fourni par conso-site-db-elec
  , cbDeleteGaz  :: Connection -> IO ()  -- ^ fourni par conso-site-db-gaz
  }

data DesinscriptionResult = DesinscriptionResult
  { drSiteId      :: SiteId
  , drSiteDeleted :: Bool
  , drSgeResults  :: [(String, Either (String, String) ())]
  } deriving (Show)


-- | Désinscrit un PRM : arrêt des services SGE + suppression données élec
-- + déliaison dans le registre. Si le site se retrouve sans PRM ni PCE,
-- il est supprimé du registre et son fichier .db est effacé.
desinscrirePrm :: Connection -> FilePath -> Bool -> Bool
               -> DesinscriptionCallbacks -> SiteId -> IO DesinscriptionResult
desinscrirePrm conn siteDbDir prod verbose callbacks siteId = do
  site <- getSite siteId
  prmT <- case srPrm site of
    Nothing      -> fail $ "Le site " <> show siteId <> " n'a pas de PRM"
    Just (p)     -> return (unPrm p)
  logV verbose $ "SGE : arrêt des services pour PRM " <> T.unpack prmT
  actifMap <- rechercherServicesActifs prod verbose prmT
  sgeResults <- arreterSge prod verbose prmT (Map.elems actifMap)
  siteConn <- openSiteDb siteDbDir siteId
  cbDeleteElec callbacks siteConn
  unlinkPrm conn siteId
  deleted <- nettoyerSiVide conn siteDbDir siteId
  return DesinscriptionResult
    { drSiteId      = siteId
    , drSiteDeleted = deleted
    , drSgeResults  = sgeResults
    }
  where
    getSite sid = do
      mSite <- lookupBySiteId conn sid
      maybe (fail $ "Site non trouvé : " <> show sid) return mSite

-- | Désinscrit un PCE : suppression des données gaz + déliaison dans le registre.
-- Aucun appel ADICT n'est effectué.
desinscrirePce :: Connection -> FilePath
               -> DesinscriptionCallbacks -> SiteId -> IO DesinscriptionResult
desinscrirePce conn siteDbDir callbacks siteId = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé : " <> show siteId) return mSite
  case srPce site of
    Nothing -> fail $ "Le site " <> show siteId <> " n'a pas de PCE"
    Just _  -> return ()
  siteConn <- openSiteDb siteDbDir siteId
  cbDeleteGaz callbacks siteConn
  unlinkPce conn siteId
  deleted <- nettoyerSiVide conn siteDbDir siteId
  return DesinscriptionResult
    { drSiteId      = siteId
    , drSiteDeleted = deleted
    , drSgeResults  = []
    }

-- | Supprime un site entièrement : arrêt SGE si PRM présent,
-- suppression du fichier .db et de l'entrée dans le registre.
supprimerSite :: Connection -> FilePath -> Bool -> Bool -> SiteId -> IO DesinscriptionResult
supprimerSite conn siteDbDir prod verbose siteId = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé : " <> show siteId) return mSite
  sgeResults <- case srPrm site of
    Nothing -> return []
    Just p  -> do
      let prmT = unPrm p
      logV verbose $ "SGE : arrêt des services pour PRM " <> T.unpack prmT
      actifMap <- rechercherServicesActifs prod verbose prmT
      arreterSge prod verbose prmT (Map.elems actifMap)
  deleteFromRegistry conn siteId
  let dbPath = siteDbPath siteDbDir siteId
  result <- try (removeFile dbPath) :: IO (Either SomeException ())
  case result of
    Left e  -> logV verbose $ "Avertissement : impossible de supprimer " <> dbPath <> " : " <> show e
    Right _ -> return ()
  return DesinscriptionResult
    { drSiteId      = siteId
    , drSiteDeleted = True
    , drSgeResults  = sgeResults
    }


-- ---------------------------------------------------------------------------
-- Fonctions internes

nettoyerSiVide :: Connection -> FilePath -> SiteId -> IO Bool
nettoyerSiVide conn siteDbDir siteId = do
  mSite <- lookupBySiteId conn siteId
  case mSite of
    Just site | srPrm site == Nothing && srPce site == Nothing -> do
      deleteFromRegistry conn siteId
      let dbPath = siteDbPath siteDbDir siteId
      result <- try (removeFile dbPath) :: IO (Either SomeException ())
      case result of
        Left _  -> return ()
        Right _ -> return ()
      return True
    _ -> return False


arreterSge :: Bool -> Bool -> Text -> [String] -> IO [(String, Either (String, String) ())]
arreterSge _ _ _ [] = return []
arreterSge prod verbose prmT serviceIds = do
  logV verbose $ "SGE CommanderArretServicesAccesDonnees : " <> show serviceIds
  req  <- mkInit (T.unpack prmT) Arret.SensSOUTIRAGE serviceIds
  resp <- mkWs req :: IO (Either (String, String) CommanderArretServicesAccesDonneesResponseType)
  return $ case resp of
    Left err -> map (\sid -> (sid, Left err)) serviceIds
    Right _  -> map (\sid -> (sid, Right ())) serviceIds
  where
    mkInit = if prod then Arret.initType else Arret.initTypeTest
    mkWs   = if prod then Arret.wsRequest else Arret.wsRequestTest


rechercherServicesActifs :: Bool -> Bool -> Text -> IO (Map String String)
rechercherServicesActifs prod verbose prmT = do
  logV verbose $ "SGE RechercherServicesAccesDonnees → PRM " <> T.unpack prmT
  req  <- if prod then RSD.initType prmStr else RSD.initTypeTest prmStr
  resp <- (if prod then RSD.wsRequest else RSD.wsRequestTest) req
            :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
  case resp of
    Left (code, lbl) -> do
      logV verbose $ "SGE RechercherServicesAccesDonnees erreur : " <> code <> " " <> lbl
      return Map.empty
    Right r ->
      return $ Map.fromList $ mapMaybe toPair $
        maybe [] servicesSouscritsType_serviceSouscrit
          (rechercherServicesAccesDonneesReponseType_servicesSouscrits r)
  where
    prmStr = T.unpack prmT
    toPair s =
      let etats = map simpleText15 (serviceSouscritType_etatCode s)
      in if "ACTIF" `notElem` etats then Nothing
         else case serviceSouscritType_mesuresTypeCode s of
           Nothing   -> Nothing
           Just code ->
             Just ( simpleText code
                  , simpleText15 (serviceSouscritType_serviceSouscritId s)
                  )
    simpleText  (MesureTypeCodeType (Xsd.XsdString s)) = s
    simpleText15 (Chaine15Type (Xsd.XsdString s))      = s


logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()


unPrm :: Prm -> Text
unPrm (Prm t) = t
