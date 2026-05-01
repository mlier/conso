{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.SiteDB.Storage.Migration
Description : Migrations SQLite des tables gaz dans la base site

Les tables gaz sont préfixées @gaz_@ et cohabitent avec les tables
électricité (@ingestion_log@, @curve_points@, …) dans la même base @{uuid}.db@.

__Règle immuable__ : ne jamais modifier une migration déjà publiée.
-}
module Conso.Fr.Gaz.SiteDB.Storage.Migration
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
        \  id                    INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  energie_kwh           REAL,\
        \  volume_brut_m3        REAL,\
        \  volume_converti       REAL,\
        \  conversion            REAL,\
        \  pta                   REAL,\
        \  pcs                   REAL,\
        \  flag_retour_zero      INTEGER,\
        \  type_qualif           TEXT,\
        \  sens_flux             TEXT,\
        \  statut_conso          TEXT,\
        \  type_conso            TEXT,\
        \  journee_gaziere       TEXT,\
        \  debut                 TEXT NOT NULL,\
        \  debut_raison          TEXT,\
        \  debut_libelle_raison  TEXT,\
        \  debut_qualite         TEXT,\
        \  debut_statut          TEXT,\
        \  debut_index_brut      REAL,\
        \  debut_index_converti  REAL,\
        \  fin                   TEXT NOT NULL,\
        \  fin_raison            TEXT,\
        \  fin_libelle_raison    TEXT,\
        \  fin_qualite           TEXT,\
        \  fin_statut            TEXT,\
        \  fin_index_brut        REAL,\
        \  fin_index_converti    REAL,\
        \  ingestion_id          INTEGER REFERENCES gaz_ingestion_log(id),\
        \  UNIQUE(debut, fin)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_gaz_consos_debut \
        \  ON gaz_consos(debut)"

        -- Consommations informatives (même schéma que gaz_consos, flux séparé)
      , "CREATE TABLE IF NOT EXISTS gaz_consos_informatives (\
        \  id                    INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  energie_kwh           REAL,\
        \  volume_brut_m3        REAL,\
        \  volume_converti       REAL,\
        \  conversion            REAL,\
        \  pta                   REAL,\
        \  pcs                   REAL,\
        \  flag_retour_zero      INTEGER,\
        \  type_qualif           TEXT,\
        \  sens_flux             TEXT,\
        \  statut_conso          TEXT,\
        \  type_conso            TEXT,\
        \  journee_gaziere       TEXT,\
        \  debut                 TEXT NOT NULL,\
        \  debut_raison          TEXT,\
        \  debut_libelle_raison  TEXT,\
        \  debut_qualite         TEXT,\
        \  debut_statut          TEXT,\
        \  debut_index_brut      REAL,\
        \  debut_index_converti  REAL,\
        \  fin                   TEXT NOT NULL,\
        \  fin_raison            TEXT,\
        \  fin_libelle_raison    TEXT,\
        \  fin_qualite           TEXT,\
        \  fin_statut            TEXT,\
        \  fin_index_brut        REAL,\
        \  fin_index_converti    REAL,\
        \  ingestion_id          INTEGER REFERENCES gaz_ingestion_log(id),\
        \  UNIQUE(debut, fin)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_gaz_consos_inf_debut \
        \  ON gaz_consos_informatives(debut)"

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

        -- Informations contractuelles (série temporelle — 1 ligne par changement)
      , "CREATE TABLE IF NOT EXISTS gaz_infos_contractuelles (\
        \  id                              INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  date_ingestion                  TEXT NOT NULL,\
        \  ingestion_id                    INTEGER REFERENCES gaz_ingestion_log(id),\
        \  date_mes                        TEXT,\
        \  tarif_acheminement              TEXT,\
        \  date_publication                TEXT,\
        \  conso_journaliere_plafond       TEXT,\
        \  car_actuelle                    TEXT,\
        \  car_future                      TEXT,\
        \  cja                             TEXT,\
        \  cja_journaliere                 TEXT,\
        \  cja_mensuelle                   TEXT,\
        \  profil_type_actuel              TEXT,\
        \  profil_type_futur               TEXT,\
        \  date_debut_profil_type_actuel   TEXT,\
        \  date_fin_profil_type_actuel     TEXT,\
        \  modulation_assiette             TEXT,\
        \  modulation_n_1                  TEXT,\
        \  modulation_n_2                  TEXT,\
        \  modulation_n_3                  TEXT\
        \)"

        -- Informations techniques
      , "CREATE TABLE IF NOT EXISTS gaz_infos_techniques (\
        \  id                              INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  date_ingestion                  TEXT NOT NULL,\
        \  ingestion_id                    INTEGER REFERENCES gaz_ingestion_log(id),\
        \  numero_rue                      TEXT,\
        \  nom_rue                         TEXT,\
        \  complement_adresse              TEXT,\
        \  code_postal                     TEXT,\
        \  commune                         TEXT,\
        \  client_sensible_mig             TEXT,\
        \  code_calibre                    TEXT,\
        \  code_debit                      TEXT,\
        \  code_debit_normalise            TEXT,\
        \  frequence                       TEXT,\
        \  matricule_compteur              TEXT,\
        \  pression_livraison              TEXT,\
        \  identifiant_pitd                TEXT,\
        \  libelle_pitd                    TEXT,\
        \  regime_propriete_compteur       TEXT,\
        \  regime_propriete_convertisseur  TEXT,\
        \  regime_propriete_enregistreur   TEXT,\
        \  regime_propriete_poste          TEXT\
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
