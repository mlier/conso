{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.SiteDB.Registry.Operations
Description : Opérations CRUD sur le registre central des sites

Fonctions pour créer, rechercher et lier des sites dans le registre central.
Toutes les fonctions prennent une connexion SQLite ouverte sur @registry.db@.
-}
module Conso.Fr.SiteDB.Registry.Operations
  ( createSite
  , lookupByPrm
  , lookupByPce
  , lookupOrCreateByPrm
  , lookupOrCreateByPce
  , linkPce
  , linkPrm
  , listSites
  ) where

import           Database.SQLite.Simple
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID4
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime, formatTime, defaultTimeLocale)
import           Conso.Fr.SiteDB.Types    (SiteId(..), Prm(..), Pce(..), SiteLabel, SiteRef(..))

-- | Génère un UUID v4, insère le site dans le registre et retourne son 'SiteId'.
-- Au moins un de @mPrm@ ou @mPce@ doit être @Just@.
createSite :: Connection -> Maybe Prm -> Maybe Pce -> Maybe SiteLabel -> IO SiteId
createSite conn mPrm mPce mLabel = do
  uuid <- UUID4.nextRandom
  now  <- getCurrentTime
  let createdAt = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      uuidText  = UUID.toText uuid
      prmText   = fmap (\(Prm p) -> p) mPrm
      pceText   = fmap (\(Pce p) -> p) mPce
  execute conn
    "INSERT INTO site_registry (uuid, prm, pce, label, created_at) VALUES (?,?,?,?,?)"
    (uuidText, prmText, pceText, mLabel, T.pack createdAt)
  return (SiteId uuid)

-- | Recherche un site par son PRM. Retourne @Nothing@ s'il n'existe pas.
lookupByPrm :: Connection -> Prm -> IO (Maybe SiteId)
lookupByPrm conn (Prm prm) = do
  rows <- query conn
    "SELECT uuid FROM site_registry WHERE prm = ?" (Only prm) :: IO [Only Text]
  return $ case rows of
    [Only uuidText] -> SiteId <$> UUID.fromText uuidText
    _               -> Nothing

-- | Recherche un site par son PCE. Retourne @Nothing@ s'il n'existe pas.
lookupByPce :: Connection -> Pce -> IO (Maybe SiteId)
lookupByPce conn (Pce pce) = do
  rows <- query conn
    "SELECT uuid FROM site_registry WHERE pce = ?" (Only pce) :: IO [Only Text]
  return $ case rows of
    [Only uuidText] -> SiteId <$> UUID.fromText uuidText
    _               -> Nothing

-- | Retourne le 'SiteId' associé au PRM, en créant le site s'il n'existe pas.
lookupOrCreateByPrm :: Connection -> Prm -> IO SiteId
lookupOrCreateByPrm conn prm = do
  mSite <- lookupByPrm conn prm
  case mSite of
    Just siteId -> return siteId
    Nothing     -> createSite conn (Just prm) Nothing Nothing

-- | Retourne le 'SiteId' associé au PCE, en créant le site s'il n'existe pas.
lookupOrCreateByPce :: Connection -> Pce -> IO SiteId
lookupOrCreateByPce conn pce = do
  mSite <- lookupByPce conn pce
  case mSite of
    Just siteId -> return siteId
    Nothing     -> createSite conn Nothing (Just pce) Nothing

-- | Associe un PCE à un site existant identifié par son 'SiteId'.
linkPce :: Connection -> SiteId -> Pce -> IO ()
linkPce conn (SiteId uuid) (Pce pce) =
  execute conn
    "UPDATE site_registry SET pce = ? WHERE uuid = ?"
    (pce, UUID.toText uuid)

-- | Associe un PRM à un site existant identifié par son 'SiteId'.
linkPrm :: Connection -> SiteId -> Prm -> IO ()
linkPrm conn (SiteId uuid) (Prm prm) =
  execute conn
    "UPDATE site_registry SET prm = ? WHERE uuid = ?"
    (prm, UUID.toText uuid)

-- | Liste tous les sites du registre.
listSites :: Connection -> IO [SiteRef]
listSites conn = do
  rows <- query_ conn
    "SELECT uuid, prm, pce, label FROM site_registry ORDER BY created_at"
    :: IO [(Text, Maybe Text, Maybe Text, Maybe Text)]
  return [ SiteRef
             { srSiteId = SiteId (parseUuid u)
             , srPrm    = Prm <$> p
             , srPce    = Pce <$> c
             , srLabel  = l
             }
         | (u, p, c, l) <- rows
         ]
  where
    parseUuid t = case UUID.fromText t of
      Just u  -> u
      Nothing -> error $ "UUID invalide dans le registre : " ++ T.unpack t
