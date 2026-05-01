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
  , insertGazConsoInformative
  , insertGazConsosInformatives
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
    "INSERT OR REPLACE INTO gaz_conso \
    \ (energie_kwh, volume_brut_m3, volume_converti, conversion, pta, pcs,\
    \  flag_retour_zero, type_qualif, sens_flux, statut_conso,\
    \  type_conso, journee_gaziere,\
    \  debut, debut_raison, debut_libelle_raison, debut_qualite, debut_statut,\
    \  debut_index_brut, debut_index_converti, fin,\
    \  fin_raison, fin_libelle_raison, fin_qualite, fin_statut,\
    \  fin_index_brut, fin_index_converti, ingestion_id)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    (  ( gcEnergie c, gcVolumeBrut c, gcVolumeConverti c
       , gcConversion c, gcPta c, gcPcs c
       , gcFlagRetourZero c, gcTypeQualif c, gcSensFlux c, gcStatutConso c
       )
    :. ( gcTypeConso c, gcJourneeGaziere c
       , gcDebut c, gcDebutRaison c, gcDebutLibelleRaison c
       , gcDebutQualite c, gcDebutStatut c
       , gcDebutIndexBrut c, gcDebutIndexConverti c, gcFin c
       )
    :. ( gcFinRaison c, gcFinLibelleRaison c, gcFinQualite c, gcFinStatut c
       , gcFinIndexBrut c, gcFinIndexConverti c, ingId
       )
    )

-- | Insère une liste de consommations dans une transaction atomique.
insertGazConsos :: Connection -> GazIngestionId -> [GazConso] -> IO ()
insertGazConsos conn ingId consos =
  withTransaction conn $ mapM_ (insertGazConso conn ingId) consos

-- | Insère une consommation informative (INSERT OR REPLACE — idempotent sur la clé unique).
insertGazConsoInformative :: Connection -> GazIngestionId -> GazConso -> IO ()
insertGazConsoInformative conn ingId c =
  execute conn
    "INSERT OR REPLACE INTO gaz_conso_informative \
    \ (energie_kwh, volume_brut_m3, volume_converti, conversion, pta, pcs,\
    \  flag_retour_zero, type_qualif, sens_flux, statut_conso,\
    \  type_conso, journee_gaziere,\
    \  debut, debut_raison, debut_libelle_raison, debut_qualite, debut_statut,\
    \  debut_index_brut, debut_index_converti, fin,\
    \  fin_raison, fin_libelle_raison, fin_qualite, fin_statut,\
    \  fin_index_brut, fin_index_converti, ingestion_id)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    (  ( gcEnergie c, gcVolumeBrut c, gcVolumeConverti c
       , gcConversion c, gcPta c, gcPcs c
       , gcFlagRetourZero c, gcTypeQualif c, gcSensFlux c, gcStatutConso c
       )
    :. ( gcTypeConso c, gcJourneeGaziere c
       , gcDebut c, gcDebutRaison c, gcDebutLibelleRaison c
       , gcDebutQualite c, gcDebutStatut c
       , gcDebutIndexBrut c, gcDebutIndexConverti c, gcFin c
       )
    :. ( gcFinRaison c, gcFinLibelleRaison c, gcFinQualite c, gcFinStatut c
       , gcFinIndexBrut c, gcFinIndexConverti c, ingId
       )
    )

-- | Insère une liste de consommations informatives dans une transaction atomique.
insertGazConsosInformatives :: Connection -> GazIngestionId -> [GazConso] -> IO ()
insertGazConsosInformatives conn ingId consos =
  withTransaction conn $ mapM_ (insertGazConsoInformative conn ingId) consos

-- | Insère une injection gaz (INSERT OR REPLACE — idempotent sur la clé unique).
insertGazInjection :: Connection -> GazIngestionId -> GazInjection -> IO ()
insertGazInjection conn ingId i =
  execute conn
    "INSERT OR REPLACE INTO gaz_injection \
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

-- | Insère des informations contractuelles (INSERT simple — 1 ligne par changement).
insertGazInfosContractuelles :: Connection -> GazIngestionId -> UTCTime -> GazInfosContractuelles -> IO ()
insertGazInfosContractuelles conn ingId now ic = do
  let now' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  execute conn
    "INSERT INTO gaz_info_contractuelle \
    \ (date_ingestion, ingestion_id,\
    \  date_mes, tarif_acheminement, date_publication, conso_journaliere_plafond,\
    \  car_actuelle, car_future,\
    \  cja, cja_journaliere, cja_mensuelle,\
    \  profil_type_actuel, profil_type_futur,\
    \  date_debut_profil_type_actuel, date_fin_profil_type_actuel,\
    \  modulation_assiette, modulation_n_1, modulation_n_2, modulation_n_3)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    (  ( now', ingId
       , icDateMes ic, icTarifAcheminement ic, icDatePublication ic
       , icConsoJournalierePlafond ic, icCarActuelle ic, icCarFuture ic
       , icCja ic, icCjaJournaliere ic
       )
    :. ( icCjaMensuelle ic
       , icProfilTypeActuel ic, icProfilTypeFutur ic
       , icDateDebutProfilTypeActuel ic, icDateFinProfilTypeActuel ic
       , icModulationAssiette ic, icModulationN1 ic, icModulationN2 ic, icModulationN3 ic
       )
    )

-- | Insère des informations techniques (INSERT simple — historique conservé).
insertGazInfosTechniques :: Connection -> GazIngestionId -> UTCTime -> GazInfosTechniques -> IO ()
insertGazInfosTechniques conn ingId now it = do
  let now' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  execute conn
    "INSERT INTO gaz_info_technique \
    \ (date_ingestion, ingestion_id,\
    \  numero_rue, nom_rue, complement_adresse, code_postal, commune,\
    \  client_sensible_mig, code_calibre, code_debit,\
    \  code_debit_normalise, frequence, matricule_compteur, pression_livraison,\
    \  identifiant_pitd, libelle_pitd,\
    \  regime_propriete_compteur, regime_propriete_convertisseur,\
    \  regime_propriete_enregistreur, regime_propriete_poste)\
    \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
    (  ( now', ingId
       , itNumeroRue it, itNomRue it, itComplementAdresse it, itCodePostal it, itCommune it
       , itClientSensibleMig it, itCodeCalibre it, itCodeDebit it
       )
    :. ( itCodeDebitNormalise it, itFrequence it, itMatriculeCompteur it, itPressionLivraison it
       , itIdentifiantPitd it, itLibellePitd it
       , itRegimeProprieteCompteur it, itRegimeProprieteConvertisseur it
       , itRegimeProprieteEnregistreur it, itRegimeProprietePoste it
       )
    )
