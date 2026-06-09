{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Desinscription
  ( DesinscriptionCallbacks(..)
  , DesinscriptionResult(..)
  , desinscrirePrm
  , desinscrirePce
  , supprimerSite
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import System.Directory (removeFile, removeDirectory)
import System.FilePath  (takeDirectory)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, SomeException)

import Database.SQLite.Simple (Connection)

import Conso.Fr.SiteDB.Types (SiteId(..), Prm(..), SiteRef(..))
import Conso.Fr.SiteDB.Storage.Connection (siteDbPath, openSiteDb)
import Conso.Fr.SiteDB.Registry.Operations
  ( lookupBySiteId, unlinkPrm, unlinkPce, deleteFromRegistry )


-- | Callbacks fournis par les extensions pour les opérations dépendantes des APIs.
data DesinscriptionCallbacks = DesinscriptionCallbacks
  { cbDeleteElec  :: Connection -> IO ()
  , cbDeleteGaz   :: Connection -> IO ()
  , cbArreterSge  :: Text -> IO [(String, Either (String, String) ())]
  }

data DesinscriptionResult = DesinscriptionResult
  { drSiteId      :: SiteId
  , drSiteDeleted :: Bool
  , drSgeResults  :: [(String, Either (String, String) ())]
  } deriving (Show)


-- | Désinscrit un PRM : arrêt des services SGE + suppression données élec
-- + déliaison dans le registre. Si le site n'a plus ni PRM ni PCE, il est supprimé.
desinscrirePrm :: Connection -> FilePath -> Bool -> Bool
               -> DesinscriptionCallbacks -> SiteId -> IO DesinscriptionResult
desinscrirePrm conn siteDbDir _prod verbose callbacks siteId = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé : " <> show siteId) return mSite
  prmT  <- case srPrm site of
    Nothing -> fail $ "Le site " <> show siteId <> " n'a pas de PRM"
    Just p  -> return (unPrm p)
  logV verbose $ "SGE : arrêt des services pour PRM " <> T.unpack prmT
  sgeResults <- cbArreterSge callbacks prmT
  siteConn <- openSiteDb siteDbDir siteId
  cbDeleteElec callbacks siteConn
  unlinkPrm conn siteId
  deleted <- nettoyerSiVide conn siteDbDir siteId
  return DesinscriptionResult
    { drSiteId      = siteId
    , drSiteDeleted = deleted
    , drSgeResults  = sgeResults
    }

-- | Désinscrit un PCE : suppression des données gaz + déliaison dans le registre.
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
supprimerSite :: Connection -> FilePath -> Bool -> Bool
              -> DesinscriptionCallbacks -> SiteId -> IO DesinscriptionResult
supprimerSite conn siteDbDir _prod verbose callbacks siteId = do
  mSite <- lookupBySiteId conn siteId
  site  <- maybe (fail $ "Site non trouvé : " <> show siteId) return mSite
  sgeResults <- case srPrm site of
    Nothing -> return []
    Just p  -> do
      let prmT = unPrm p
      logV verbose $ "SGE : arrêt des services pour PRM " <> T.unpack prmT
      cbArreterSge callbacks prmT
  deleteFromRegistry conn siteId
  let dbPath    = siteDbPath siteDbDir siteId
      shard2Dir = takeDirectory dbPath
      shard1Dir = takeDirectory shard2Dir
  mapM_ (\p -> try (removeFile p) :: IO (Either SomeException ()))
    [dbPath, dbPath <> "-shm", dbPath <> "-wal"]
  mapM_ (\d -> try (removeDirectory d) :: IO (Either SomeException ()))
    [shard2Dir, shard1Dir]
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
    Just site | isNothing (srPrm site) && isNothing (srPce site) -> do
      deleteFromRegistry conn siteId
      let dbPath    = siteDbPath siteDbDir siteId
          shard2Dir = takeDirectory dbPath
          shard1Dir = takeDirectory shard2Dir
      mapM_ (\p -> try (removeFile p) :: IO (Either SomeException ()))
        [dbPath, dbPath <> "-shm", dbPath <> "-wal"]
      mapM_ (\d -> try (removeDirectory d) :: IO (Either SomeException ()))
        [shard2Dir, shard1Dir]
      return True
    _ -> return False


logV :: Bool -> String -> IO ()
logV True  msg = hPutStrLn stderr $ "[verbose] " <> msg
logV False _   = return ()


unPrm :: Prm -> Text
unPrm (Prm t) = t
