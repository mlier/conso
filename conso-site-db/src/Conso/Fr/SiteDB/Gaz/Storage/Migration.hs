{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.SiteDB.Gaz.Storage.Migration
Description : Migrations SQLite des tables gaz dans la base site

Les tables gaz sont préfixées @gaz_@ et cohabitent avec les tables
électricité (@ingestion_log@, @curve_points@, …) dans la même base @{uuid}.db@.

__Règle immuable__ : ne jamais modifier une migration déjà publiée.
-}
module Conso.Fr.SiteDB.Gaz.Storage.Migration
  ( gazMigrations
  , currentGazSchemaVersion
  , ensureGazSchema
  ) where

import Database.SQLite.Simple
import Control.Monad (when, forM_)

-- | Version courante du schéma gaz.
currentGazSchemaVersion :: Int
currentGazSchemaVersion = 1

-- | Migrations gaz indexées par numéro de version.
gazMigrations :: [(Int, [Query])]
gazMigrations =
  [ (1,
      [ "INSERT OR REPLACE INTO gaz_schema_version VALUES (1)"

        -- Log d'ingestion gaz
      , "CREATE TABLE IF NOT EXISTS gaz_ingestion_log (\
        \  id              INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  endpoint        TEXT NOT NULL,\
        \  date_debut      TEXT,\
        \  date_fin        TEXT,\
        \  periode         TEXT,\
        \  type_donnee     TEXT,\
        \  date_ingestion  TEXT NOT NULL,\
        \  nb_lignes       INTEGER\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_gaz_ingestion_endpoint \
        \  ON gaz_ingestion_log(endpoint, date_ingestion)"

        -- Consommations (publiées + informatives)
      , "CREATE TABLE IF NOT EXISTS gaz_consos (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  date_debut        TEXT NOT NULL,\
        \  date_fin          TEXT NOT NULL,\
        \  periode           TEXT NOT NULL,\
        \  type_donnee       TEXT NOT NULL,\
        \  energie_kwh       REAL,\
        \  volume_brut_m3    REAL,\
        \  volume_converti   REAL,\
        \  coeff_conversion  REAL,\
        \  coeff_pta         REAL,\
        \  raw_json          TEXT NOT NULL,\
        \  ingestion_id      INTEGER REFERENCES gaz_ingestion_log(id),\
        \  UNIQUE(date_debut, date_fin, type_donnee, periode)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_gaz_consos_dates \
        \  ON gaz_consos(date_debut, date_fin)"
      , "CREATE INDEX IF NOT EXISTS idx_gaz_consos_type \
        \  ON gaz_consos(type_donnee, periode)"

        -- Injections publiées
      , "CREATE TABLE IF NOT EXISTS gaz_injections (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  date_debut        TEXT NOT NULL,\
        \  date_fin          TEXT NOT NULL,\
        \  periode           TEXT NOT NULL,\
        \  type_donnee       TEXT NOT NULL,\
        \  energie_kwh       REAL,\
        \  volume_brut_m3    REAL,\
        \  volume_converti   REAL,\
        \  raw_json          TEXT NOT NULL,\
        \  ingestion_id      INTEGER REFERENCES gaz_ingestion_log(id),\
        \  UNIQUE(date_debut, date_fin, type_donnee, periode)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_gaz_injections_dates \
        \  ON gaz_injections(date_debut, date_fin)"

        -- Informations contractuelles
      , "CREATE TABLE IF NOT EXISTS gaz_infos_contractuelles (\
        \  id              INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  date_debut      TEXT,\
        \  date_fin        TEXT,\
        \  segment_client  TEXT,\
        \  num_compteur    TEXT,\
        \  tarif           TEXT,\
        \  raw_json        TEXT NOT NULL,\
        \  date_ingestion  TEXT NOT NULL,\
        \  ingestion_id    INTEGER REFERENCES gaz_ingestion_log(id)\
        \)"

        -- Informations techniques
      , "CREATE TABLE IF NOT EXISTS gaz_infos_techniques (\
        \  id              INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  type_compteur   TEXT,\
        \  pression        TEXT,\
        \  date_releve     TEXT,\
        \  etat_compteur   TEXT,\
        \  raw_json        TEXT NOT NULL,\
        \  date_ingestion  TEXT NOT NULL,\
        \  ingestion_id    INTEGER REFERENCES gaz_ingestion_log(id)\
        \)"
      ])
  ]

-- | Applique les migrations gaz manquantes.
-- Utilise une table @gaz_schema_version@ séparée de la table élec.
ensureGazSchema :: Connection -> IO ()
ensureGazSchema conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS gaz_schema_version (version INTEGER NOT NULL)"
  rows <- query_ conn "SELECT version FROM gaz_schema_version" :: IO [Only Int]
  let currentVersion = case rows of
        []       -> 0
        [Only v] -> v
        _        -> error "gaz_schema_version corrompue : plusieurs lignes"
  when (currentVersion < currentGazSchemaVersion) $
    withTransaction conn $ do
      let pending = filter (\(v, _) -> v > currentVersion) gazMigrations
      forM_ pending $ \(_, sqls) ->
        mapM_ (execute_ conn) sqls
