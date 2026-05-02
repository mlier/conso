{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.SiteDB.Registry.Schema
Description : Schéma SQLite du registre central des sites

Le registre central (@registry.db@) contient une table unique @site_registry@
qui fait le lien entre l'UUID d'un site et ses identifiants métier (PRM, PCE).

__Règle immuable__ : ne jamais modifier une migration déjà publiée.
Pour évoluer le schéma, ajouter une nouvelle migration numérotée.
-}
module Conso.Fr.SiteDB.Registry.Schema
  ( ensureRegistrySchema
  , currentRegistryVersion
  ) where

import Database.SQLite.Simple
import Control.Monad (when, forM_)

-- | Version courante du schéma du registre.
currentRegistryVersion :: Int
currentRegistryVersion = 1

-- | Migrations du registre, indexées par numéro de version.
registryMigrations :: [(Int, [Query])]
registryMigrations =
  [ (1,
      [ "INSERT OR REPLACE INTO schema_version VALUES (1)"

        -- Table principale du registre
      , "CREATE TABLE IF NOT EXISTS site_registry (\
        \  uuid                  TEXT PRIMARY KEY,\
        \  prm                   TEXT UNIQUE,\
        \  pce                   TEXT UNIQUE,\
        \  label                 TEXT,\
        \  created_at            TEXT NOT NULL,\
        \  gaz_avec_injections   INTEGER NOT NULL DEFAULT 0\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_registry_prm \
        \  ON site_registry(prm)"
      , "CREATE INDEX IF NOT EXISTS idx_registry_pce \
        \  ON site_registry(pce)"
      ])
  ]

-- | Applique les migrations manquantes du registre.
-- Crée la table @schema_version@ si absente, puis exécute les migrations
-- manquantes dans une transaction atomique.
ensureRegistrySchema :: Connection -> IO ()
ensureRegistrySchema conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL)"
  rows <- query_ conn "SELECT version FROM schema_version" :: IO [Only Int]
  let currentVersion = case rows of
        []       -> 0
        [Only v] -> v
        _        -> error "registry schema_version corrompue : plusieurs lignes"
  when (currentVersion < currentRegistryVersion) $
    withTransaction conn $ do
      let pending = filter (\(v, _) -> v > currentVersion) registryMigrations
      forM_ pending $ \(_, sqls) ->
        mapM_ (execute_ conn) sqls
