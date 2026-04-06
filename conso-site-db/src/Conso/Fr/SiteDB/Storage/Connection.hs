{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.SiteDB.Storage.Connection
Description : Connexion SQLite d'un site (nommé par UUID) — core pur sans migrations

Ouvre la base SQLite d'un site identifié par son UUID.
Le fichier est stocké à plat : @siteDbDir/{uuid}.db@

Ce module est le core pur : il ne dépend d'aucun schéma métier (élec ou gaz).
Les extensions élec et gaz appellent ce module puis appliquent leurs propres migrations.
-}
module Conso.Fr.SiteDB.Storage.Connection
  ( siteDbPath
  , openSiteDb
  , configurePragmas
  ) where

import           Database.SQLite.Simple
import           System.FilePath        ((</>), (<.>), takeDirectory)
import           System.Directory       (createDirectoryIfMissing)
import qualified Data.UUID              as UUID
import           Conso.Fr.SiteDB.Types  (SiteId(..))

-- | Calcule le chemin SQLite d'un site : @siteDbDir/{uuid}.db@
siteDbPath :: FilePath -> SiteId -> FilePath
siteDbPath siteDbDir (SiteId uuid) = siteDbDir </> UUID.toString uuid <.> "db"

-- | Ouvre (ou crée) la base SQLite d'un site.
-- Crée le répertoire si absent, applique les PRAGMA.
-- Ne run aucune migration : c'est la responsabilité de l'extension (élec ou gaz).
openSiteDb :: FilePath -> SiteId -> IO Connection
openSiteDb siteDbDir siteId = do
  let dbPath = siteDbPath siteDbDir siteId
  createDirectoryIfMissing True (takeDirectory dbPath)
  conn <- open dbPath
  configurePragmas conn
  return conn

-- | Configure les PRAGMA SQLite optimisés pour les accès concurrents.
-- PRAGMA appliqués :
--
-- * @journal_mode = WAL@ — écriture non bloquante
-- * @synchronous = NORMAL@ — compromis performance\/durabilité
-- * @foreign_keys = ON@ — intégrité référentielle activée
-- * @busy_timeout = 5000@ — attente 5 s en cas de verrou
configurePragmas :: Connection -> IO ()
configurePragmas conn = do
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA synchronous = NORMAL"
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA busy_timeout = 5000"
