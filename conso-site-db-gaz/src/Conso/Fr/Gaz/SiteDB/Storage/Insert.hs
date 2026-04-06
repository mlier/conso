{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.SiteDB.Storage.Insert
Description : Insertions SQLite pour les données gaz

Fonctions d'insertion pour les tables @gaz_*@ et la gestion du log
d'ingestion @gaz_ingestion_log@.
-}
module Conso.Fr.Gaz.SiteDB.Storage.Insert
  ( GazIngestionId
  , logGazIngestion
  , insertGazConso
  , insertGazConsos
  , insertGazInjection
  , insertGazInjections
  , insertGazInfosContractuelles
  , insertGazInfosTechniques
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import           Data.Time              (UTCTime, formatTime, defaultTimeLocale)
import           Conso.Fr.Gaz.SiteDB.Types

-- | Identifiant d'une entrée dans @gaz_ingestion_log@.
type GazIngestionId = Int

-- | Enregistre une ingestion dans @gaz_ingestion_log@.
logGazIngestion
  :: Connection
  -> Text        -- ^ Endpoint appelé (ex. @\"donnees_consos_publiees\"@)
  -> Maybe Text  -- ^ Date de début de la requête
  -> Maybe Text  -- ^ Date de fin de la requête
  -> Maybe Text  -- ^ Période (@\"J\"@ ou @\"M\"@)
  -> Maybe Text  -- ^ Type de donnée (@\"PUBLIEE\"@, …)
  -> UTCTime     -- ^ Horodate d'ingestion
  -> Int         -- ^ Nombre de lignes insérées
  -> IO GazIngestionId
logGazIngestion conn endpoint mDateDebut mDateFin mPeriode mType now nbLignes = do
  let now' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  execute conn
    "INSERT INTO gaz_ingestion_log \
    \ (endpoint, date_debut, date_fin, periode, type_donnee, date_ingestion, nb_lignes) \
    \ VALUES (?,?,?,?,?,?,?)"
    (endpoint, mDateDebut, mDateFin, mPeriode, mType, now', nbLignes)
  fromIntegral <$> lastInsertRowId conn

-- | Insère une consommation gaz (INSERT OR REPLACE — idempotent sur la clé unique).
insertGazConso :: Connection -> GazIngestionId -> GazConso -> IO ()
insertGazConso conn ingId c =
  execute conn
    "INSERT OR REPLACE INTO gaz_consos \
    \ (date_debut, date_fin, periode, type_donnee, energie_kwh, \
    \  volume_brut_m3, volume_converti, coeff_conversion, coeff_pta, \
    \  raw_json, ingestion_id) \
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?)"
    (  ( gcDateDebut c
       , gcDateFin c
       , periodeGazToText (gcPeriode c)
       , typeDonneeToText (gcTypeDonnee c)
       , gcEnergie c
       , gcVolumeBrut c
       , gcVolumeConverti c
       , gcCoeffConversion c
       , gcCoeffPta c
       )
    :. ( gcRawJson c
       , ingId
       )
    )

-- | Insère une liste de consommations dans une transaction atomique.
insertGazConsos :: Connection -> GazIngestionId -> [GazConso] -> IO ()
insertGazConsos conn ingId consos =
  withTransaction conn $ mapM_ (insertGazConso conn ingId) consos

-- | Insère une injection gaz (INSERT OR REPLACE — idempotent sur la clé unique).
insertGazInjection :: Connection -> GazIngestionId -> GazInjection -> IO ()
insertGazInjection conn ingId i =
  execute conn
    "INSERT OR REPLACE INTO gaz_injections \
    \ (date_debut, date_fin, periode, type_donnee, energie_kwh, \
    \  volume_brut_m3, volume_converti, raw_json, ingestion_id) \
    \ VALUES (?,?,?,?,?,?,?,?,?)"
    ( giDateDebut i
    , giDateFin i
    , periodeGazToText (giPeriode i)
    , typeDonneeToText (giTypeDonnee i)
    , giEnergie i
    , giVolumeBrut i
    , giVolumeConverti i
    , giRawJson i
    , ingId
    )

-- | Insère une liste d'injections dans une transaction atomique.
insertGazInjections :: Connection -> GazIngestionId -> [GazInjection] -> IO ()
insertGazInjections conn ingId injections =
  withTransaction conn $ mapM_ (insertGazInjection conn ingId) injections

-- | Insère des informations contractuelles (INSERT simple — historique conservé).
insertGazInfosContractuelles :: Connection -> GazIngestionId -> UTCTime -> GazInfosContractuelles -> IO ()
insertGazInfosContractuelles conn ingId now ic = do
  let now' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  execute conn
    "INSERT INTO gaz_infos_contractuelles \
    \ (date_debut, date_fin, segment_client, num_compteur, tarif, \
    \  raw_json, date_ingestion, ingestion_id) \
    \ VALUES (?,?,?,?,?,?,?,?)"
    ( icDateDebut ic
    , icDateFin ic
    , icSegmentClient ic
    , icNumCompteur ic
    , icTarif ic
    , icRawJson ic
    , now'
    , ingId
    )

-- | Insère des informations techniques (INSERT simple — historique conservé).
insertGazInfosTechniques :: Connection -> GazIngestionId -> UTCTime -> GazInfosTechniques -> IO ()
insertGazInfosTechniques conn ingId now it = do
  let now' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  execute conn
    "INSERT INTO gaz_infos_techniques \
    \ (type_compteur, pression, date_releve, etat_compteur, \
    \  raw_json, date_ingestion, ingestion_id) \
    \ VALUES (?,?,?,?,?,?,?)"
    ( itTypeCompteur it
    , itPression it
    , itDateReleve it
    , itEtatCompteur it
    , itRawJson it
    , now'
    , ingId
    )
