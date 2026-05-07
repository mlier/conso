{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Migration
Description : Migrations SQLite numérotées pour les bases PRM SgeDB

Gère le versionnement du schéma SQLite. À chaque ouverture de connexion,
'ensureSchema' vérifie la table @elec_schema_version@ et applique les migrations
manquantes dans une transaction atomique.

__Règle immuable__ : ne jamais modifier une migration déjà publiée.
Pour corriger une erreur ou ajouter une colonne, ajouter une nouvelle migration
numérotée à la liste 'migrations'.
-}
module Conso.Fr.Elec.SiteDB.Storage.Migration where

import           Database.SQLite.Simple
import           Control.Monad          (when, forM_)

-- | Version courante du schéma attendue par ce code.
-- À incrémenter à chaque nouvelle migration ajoutée dans 'migrations'.
currentSchemaVersion :: Int
currentSchemaVersion = 1

-- | Liste ordonnée des migrations, indexées par numéro de version.
--
-- Pour ajouter une migration : ajouter un tuple @(n, [sql1, sql2, …])@
-- avec @n = currentSchemaVersion + 1@, puis incrémenter 'currentSchemaVersion'.
-- Ne jamais modifier les migrations existantes.
migrations :: [(Int, [Query])]
migrations =
  [ (1,
      -- ========================================
      -- Migration 1 : Schéma initial
      -- ========================================
      [ -- Table de version (créée avant, mais on la met ici aussi pour l'historique)
        "INSERT OR REPLACE INTO elec_schema_version VALUES (1)"

        -- Traçabilité des ingestions
      , "CREATE TABLE IF NOT EXISTS elec_ingestion_log (\
        \  id                 INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  code_flux          TEXT NOT NULL,\
        \  mode_publication   TEXT NOT NULL,\
        \  id_demande         TEXT NOT NULL,\
        \  id_publication     TEXT,\
        \  num_sequence       INTEGER,\
        \  date_ingestion     TEXT NOT NULL,\
        \  date_debut_periode TEXT,\
        \  date_fin_periode   TEXT,\
        \  fichier_source     TEXT\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_ingestion_flux \
        \  ON elec_ingestion_log(code_flux, date_ingestion)"

        -- Courbes de charge (R63, R63A, R63B)
      , "CREATE TABLE IF NOT EXISTS elec_curve_points (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier      TEXT NOT NULL,\
        \  grandeur_metier   TEXT NOT NULL,\
        \  grandeur_physique TEXT NOT NULL,\
        \  unite             TEXT NOT NULL,\
        \  horodate          TEXT NOT NULL,\
        \  valeur            TEXT NOT NULL,\
        \  pas               TEXT NOT NULL,\
        \  nature            TEXT NOT NULL,\
        \  type_completion   TEXT,\
        \  iv                INTEGER,\
        \  ec                INTEGER,\
        \  ingestion_id      INTEGER REFERENCES elec_ingestion_log(id),\
        \  UNIQUE(etape_metier, grandeur_metier, grandeur_physique, horodate, pas)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_curve_horodate \
        \  ON elec_curve_points(horodate)"
      , "CREATE INDEX IF NOT EXISTS idx_curve_grandeur_horodate \
        \  ON elec_curve_points(grandeur_metier, grandeur_physique, horodate)"

        -- Index (R64, R64A, R64B)
      , "CREATE TABLE IF NOT EXISTS elec_index_values (\
        \  id                   INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier         TEXT NOT NULL,\
        \  contexte_releve      TEXT NOT NULL,\
        \  type_releve          TEXT NOT NULL,\
        \  motif_releve         TEXT,\
        \  grandeur_metier      TEXT NOT NULL,\
        \  grandeur_physique    TEXT NOT NULL,\
        \  unite                TEXT NOT NULL,\
        \  id_calendrier        TEXT,\
        \  libelle_grille       TEXT,\
        \  id_classe_temporelle TEXT,\
        \  libelle_classe_temp  TEXT,\
        \  code_cadran          TEXT,\
        \  is_totalisateur      INTEGER NOT NULL DEFAULT 0,\
        \  horodate             TEXT NOT NULL,\
        \  valeur               INTEGER NOT NULL,\
        \  iv                   INTEGER,\
        \  ingestion_id         INTEGER REFERENCES elec_ingestion_log(id)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_index_horodate \
        \  ON elec_index_values(horodate)"
      , "CREATE INDEX IF NOT EXISTS idx_index_contexte_horodate \
        \  ON elec_index_values(contexte_releve, type_releve, horodate)"
      , "CREATE INDEX IF NOT EXISTS idx_index_grandeur \
        \  ON elec_index_values(grandeur_metier, grandeur_physique, horodate)"

        -- Energies quotidiennes (R65)
      , "CREATE TABLE IF NOT EXISTS elec_daily_energy (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier      TEXT NOT NULL,\
        \  grandeur_metier   TEXT NOT NULL,\
        \  grandeur_physique TEXT NOT NULL,\
        \  unite             TEXT NOT NULL,\
        \  mode_calcul       TEXT NOT NULL,\
        \  date       TEXT NOT NULL,\
        \  valeur            TEXT NOT NULL,\
        \  ingestion_id      INTEGER REFERENCES elec_ingestion_log(id),\
        \  UNIQUE(grandeur_metier, grandeur_physique, date)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_energy_date \
        \  ON elec_daily_energy(date)"

        -- Pmax quotidiennes (R66, R66B)
      , "CREATE TABLE IF NOT EXISTS elec_daily_pmax (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier      TEXT NOT NULL,\
        \  grandeur_metier   TEXT NOT NULL,\
        \  grandeur_physique TEXT NOT NULL,\
        \  unite             TEXT NOT NULL,\
        \  horodate          TEXT NOT NULL,\
        \  valeur            TEXT NOT NULL,\
        \  ingestion_id      INTEGER REFERENCES elec_ingestion_log(id),\
        \  UNIQUE(grandeur_metier, grandeur_physique, horodate)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_pmax_horodate \
        \  ON elec_daily_pmax(horodate)"

        -- Mesures facturantes (R67)
      , "CREATE TABLE IF NOT EXISTS elec_billing_measures (\
        \  id                    INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier          TEXT NOT NULL,\
        \  id_motif_releve       TEXT NOT NULL,\
        \  libelle_motif_releve  TEXT NOT NULL,\
        \  grandeur_metier       TEXT NOT NULL,\
        \  grandeur_physique     TEXT NOT NULL,\
        \  unite                 TEXT NOT NULL,\
        \  code_grille           TEXT,\
        \  libelle_grille        TEXT NOT NULL,\
        \  code_calendrier       TEXT,\
        \  libelle_calendrier    TEXT NOT NULL,\
        \  id_classe_temporelle  TEXT NOT NULL,\
        \  libelle_classe_temp   TEXT NOT NULL,\
        \  date_creation         TEXT NOT NULL,\
        \  debut                 TEXT NOT NULL,\
        \  fin                   TEXT NOT NULL,\
        \  quantite              INTEGER NOT NULL,\
        \  code_nature           TEXT,\
        \  libelle_nature        TEXT NOT NULL,\
        \  code_statut           TEXT,\
        \  libelle_statut        TEXT NOT NULL,\
        \  ingestion_id          INTEGER REFERENCES elec_ingestion_log(id)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_billing_periode \
        \  ON elec_billing_measures(debut, fin)"
      , "CREATE INDEX IF NOT EXISTS idx_billing_grandeur \
        \  ON elec_billing_measures(grandeur_metier, grandeur_physique)"

        -- Informations techniques et contractuelles (C68)
      , "CREATE TABLE IF NOT EXISTS elec_prm_info (\
        \  id                  INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  segment             TEXT,\
        \  etat_contractuel    TEXT,\
        \  etat_alimentation   TEXT,\
        \  puissance_souscrite TEXT,\
        \  domaine_tension     TEXT,\
        \  raw_json            TEXT NOT NULL,\
        \  date_ingestion      TEXT NOT NULL,\
        \  ingestion_id        INTEGER REFERENCES elec_ingestion_log(id)\
        \)"

        -- Journal des demandes M023 de backfill (déduplication)
      , "CREATE TABLE IF NOT EXISTS elec_backfill_log (\
        \  id          INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  type_mesure TEXT NOT NULL,\
        \  debut       TEXT NOT NULL,\
        \  fin         TEXT NOT NULL,\
        \  date_envoi  TEXT NOT NULL,\
        \  affaire_id  TEXT,\
        \  statut_cr   TEXT,\
        \  date_cr     TEXT\
        \)"
      ])

  -- Exemple de migration future :
  -- , (2,
  --     [ "ALTER TABLE elec_billing_measures ADD COLUMN nouveau_champ TEXT"
  --     , "UPDATE elec_schema_version SET version = 2"
  --     ])
  ]

-- | Applique les migrations manquantes de façon idempotente.
-- Crée la table @elec_schema_version@ si absente, puis exécute dans une transaction
-- toutes les migrations dont le numéro est supérieur à la version actuelle.
ensureSchema :: Connection -> IO ()
ensureSchema conn = do
  -- Table de version : créée en dehors des migrations pour bootstrapping
  execute_ conn
    "CREATE TABLE IF NOT EXISTS elec_schema_version (version INTEGER NOT NULL)"
  rows <- query_ conn "SELECT version FROM elec_schema_version" :: IO [Only Int]
  let currentVersion = case rows of
        []       -> 0
        [Only v] -> v
        _        -> error "elec_schema_version corrompue : plusieurs lignes"

  when (currentVersion < currentSchemaVersion) $
    withTransaction conn $ do
      let pending = filter (\(v, _) -> v > currentVersion) migrations
      forM_ pending $ \(_, sqls) ->
        mapM_ (execute_ conn) sqls
