{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Storage.Migration where

import           Database.SQLite.Simple
import           Control.Monad          (when, forM_)

-- | Version courante du schéma attendue par le code.
-- Incrémenter à chaque nouvelle migration ajoutée.
currentSchemaVersion :: Int
currentSchemaVersion = 1

-- | Liste ordonnée des migrations.
-- RÈGLE : ne jamais modifier une migration déjà publiée.
-- Pour corriger une erreur, ajouter une nouvelle migration.
migrations :: [(Int, [Query])]
migrations =
  [ (1,
      -- ========================================
      -- Migration 1 : Schéma initial
      -- ========================================
      [ -- Table de version (créée avant, mais on la met ici aussi pour l'historique)
        "INSERT OR REPLACE INTO schema_version VALUES (1)"

        -- Traçabilité des ingestions
      , "CREATE TABLE IF NOT EXISTS ingestion_log (\
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
        \  ON ingestion_log(code_flux, date_ingestion)"

        -- Courbes de charge (R63, R63A, R63B)
      , "CREATE TABLE IF NOT EXISTS curve_points (\
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
        \  ingestion_id      INTEGER REFERENCES ingestion_log(id),\
        \  UNIQUE(etape_metier, grandeur_metier, grandeur_physique, horodate, pas)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_curve_horodate \
        \  ON curve_points(horodate)"
      , "CREATE INDEX IF NOT EXISTS idx_curve_grandeur_horodate \
        \  ON curve_points(grandeur_metier, grandeur_physique, horodate)"

        -- Index (R64, R64A, R64B)
      , "CREATE TABLE IF NOT EXISTS index_values (\
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
        \  ingestion_id         INTEGER REFERENCES ingestion_log(id)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_index_horodate \
        \  ON index_values(horodate)"
      , "CREATE INDEX IF NOT EXISTS idx_index_contexte_horodate \
        \  ON index_values(contexte_releve, type_releve, horodate)"
      , "CREATE INDEX IF NOT EXISTS idx_index_grandeur \
        \  ON index_values(grandeur_metier, grandeur_physique, horodate)"

        -- Energies quotidiennes (R65)
      , "CREATE TABLE IF NOT EXISTS daily_energy (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier      TEXT NOT NULL,\
        \  grandeur_metier   TEXT NOT NULL,\
        \  grandeur_physique TEXT NOT NULL,\
        \  unite             TEXT NOT NULL,\
        \  mode_calcul       TEXT NOT NULL,\
        \  date_mesure       TEXT NOT NULL,\
        \  valeur            TEXT NOT NULL,\
        \  ingestion_id      INTEGER REFERENCES ingestion_log(id),\
        \  UNIQUE(grandeur_metier, grandeur_physique, date_mesure)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_energy_date \
        \  ON daily_energy(date_mesure)"

        -- Pmax quotidiennes (R66, R66B)
      , "CREATE TABLE IF NOT EXISTS daily_pmax (\
        \  id                INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  etape_metier      TEXT NOT NULL,\
        \  grandeur_metier   TEXT NOT NULL,\
        \  grandeur_physique TEXT NOT NULL,\
        \  unite             TEXT NOT NULL,\
        \  horodate          TEXT NOT NULL,\
        \  valeur            TEXT NOT NULL,\
        \  ingestion_id      INTEGER REFERENCES ingestion_log(id),\
        \  UNIQUE(grandeur_metier, grandeur_physique, horodate)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_pmax_horodate \
        \  ON daily_pmax(horodate)"

        -- Mesures facturantes (R67)
      , "CREATE TABLE IF NOT EXISTS billing_measures (\
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
        \  dbt_mesure            TEXT NOT NULL,\
        \  fin_mesure            TEXT NOT NULL,\
        \  quantite              INTEGER NOT NULL,\
        \  code_nature           TEXT,\
        \  libelle_nature        TEXT NOT NULL,\
        \  code_statut           TEXT,\
        \  libelle_statut        TEXT NOT NULL,\
        \  ingestion_id          INTEGER REFERENCES ingestion_log(id)\
        \)"
      , "CREATE INDEX IF NOT EXISTS idx_billing_periode \
        \  ON billing_measures(dbt_mesure, fin_mesure)"
      , "CREATE INDEX IF NOT EXISTS idx_billing_grandeur \
        \  ON billing_measures(grandeur_metier, grandeur_physique)"

        -- Informations techniques et contractuelles (C68)
      , "CREATE TABLE IF NOT EXISTS prm_info (\
        \  id                  INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  segment             TEXT,\
        \  etat_contractuel    TEXT,\
        \  etat_alimentation   TEXT,\
        \  puissance_souscrite TEXT,\
        \  domaine_tension     TEXT,\
        \  raw_json            TEXT NOT NULL,\
        \  date_ingestion      TEXT NOT NULL,\
        \  ingestion_id        INTEGER REFERENCES ingestion_log(id)\
        \)"
      ])

  -- Exemple de migration future :
  -- , (2,
  --     [ "ALTER TABLE billing_measures ADD COLUMN nouveau_champ TEXT"
  --     , "UPDATE schema_version SET version = 2"
  --     ])
  ]

-- | Applique les migrations manquantes.
-- À appeler à chaque ouverture de connexion (via openPrmDb).
ensureSchema :: Connection -> IO ()
ensureSchema conn = do
  -- Table de version : créée en dehors des migrations pour bootstrapping
  execute_ conn
    "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER NOT NULL)"
  rows <- query_ conn "SELECT version FROM schema_version" :: IO [Only Int]
  let currentVersion = case rows of
        []       -> 0
        [Only v] -> v
        _        -> error "schema_version corrompue : plusieurs lignes"

  when (currentVersion < currentSchemaVersion) $
    withTransaction conn $ do
      let pending = filter (\(v, _) -> v > currentVersion) migrations
      forM_ pending $ \(_, sqls) ->
        mapM_ (execute_ conn) sqls
