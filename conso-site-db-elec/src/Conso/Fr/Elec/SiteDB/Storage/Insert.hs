{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Insert
Description : Insertions SQLite pour tous les types de flux SgeDB

Fournit une fonction d'insertion par type de flux (R63..R67, C68)
et la fonction 'logIngestion' pour tracer chaque ingestion dans @elec_ingestion_log@.

Stratégies d'insertion :

  * @elec_curve_points@, @elec_daily_energy@, @elec_daily_pmax@ — @INSERT OR REPLACE@ (idempotent)
  * @elec_index_values@, @elec_billing_measures@, @elec_prm_info@ — @INSERT@ simple (conserve l'historique)
-}
module Conso.Fr.Elec.SiteDB.Storage.Insert
  ( insertCurvePoints
  , insertIndexValues
  , insertDailyEnergy
  , insertDailyPmax
  , insertBillingMeasures
  , insertPrmInfo
  , logIngestion
  , IngestionId
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime, Day)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           Data.Time.Calendar     (showGregorian)
import           Conso.Fr.Elec.SiteDB.Types.Common
import           Conso.Fr.Elec.SiteDB.Types.Header  (CodeFlux, codeFluxToText)
import           Conso.Fr.Elec.SiteDB.Types.R63
import           Conso.Fr.Elec.SiteDB.Types.R64
import           Conso.Fr.Elec.SiteDB.Types.R65
import           Conso.Fr.Elec.SiteDB.Types.R66
import           Conso.Fr.Elec.SiteDB.Types.R67
import           Conso.Fr.Elec.SiteDB.Types.C68
import           Data.Aeson                        (encode)
import qualified Data.ByteString.Lazy              as BL
import qualified Data.Text.Encoding                as TE

type IngestionId = Int

-- ---------------------------------------------------------------------------
-- Helpers de formatage

fmtUTC :: UTCTime -> Text
fmtUTC = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

fmtDay :: Day -> Text
fmtDay = T.pack . showGregorian

-- ---------------------------------------------------------------------------
-- Log d'ingestion

-- | Enregistre une ligne dans @elec_ingestion_log@ et retourne son identifiant.
logIngestion
  :: Connection    -- ^ Connexion à la base PRM
  -> CodeFlux      -- ^ Type de flux ingéré
  -> Text          -- ^ Mode de publication (@P@, @Q@, @H@ ou @M@)
  -> Text          -- ^ Identifiant de la demande SGE
  -> Maybe Text    -- ^ Identifiant de publication (R6X-REC uniquement)
  -> Maybe Int     -- ^ Numéro de séquence (optionnel)
  -> UTCTime       -- ^ Horodate d'ingestion (@getCurrentTime@)
  -> Maybe UTCTime -- ^ Début de la période couverte
  -> Maybe UTCTime -- ^ Fin de la période couverte
  -> Maybe Text    -- ^ Nom du fichier source (pour traçabilité)
  -> IO IngestionId
logIngestion conn cf modePub idDem idPub numSeq dateIng deb fin src = do
  execute conn
    "INSERT INTO elec_ingestion_log \
    \ (code_flux, mode_publication, id_demande, id_publication, \
    \  num_sequence, date_ingestion, date_debut_periode, date_fin_periode, \
    \  fichier_source) \
    \ VALUES (?,?,?,?,?,?,?,?,?)"
    ( codeFluxToText cf, modePub, idDem, idPub, numSeq
    , fmtUTC dateIng
    , fmap fmtUTC deb, fmap fmtUTC fin
    , src )
  fmap fromIntegral (lastInsertRowId conn)

-- ---------------------------------------------------------------------------
-- Insertion courbes de charge (R63)

-- | Insère les points de courbe de charge d'une 'MesureR63' dans @elec_curve_points@.
-- Utilise @INSERT OR REPLACE@ : idempotent sur la clé @(etape_metier, grandeur_metier, grandeur_physique, horodate, pas)@.
insertCurvePoints :: Connection -> IngestionId -> MesureR63 -> IO ()
insertCurvePoints conn ingId m =
  mapM_ (insertGrandeur (etapeMetierToText (mr63EtapeMetier m))) (mr63Grandeurs m)
  where
    insertGrandeur em g =
      mapM_ (insertPoint em g) (gr63Points g)
    insertPoint em g p =
      execute conn
        "INSERT OR REPLACE INTO elec_curve_points \
        \ (etape_metier, grandeur_metier, grandeur_physique, unite, \
        \  horodate, valeur, pas, nature, type_completion, iv, ec, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
        (( em
        , grandeurMetierToText (gr63GrandeurMetier g)
        , grandeurPhysiqueR63ToText (gr63GrandeurPhysique g)
        , gr63Unite g
        , fmtUTC (pcHorodate p)
        , pcValeur p
        , pasToText (pcPas p)
        , naturePointToText (pcNature p)
        , fmap typeCompletionToText (pcTypeCompletion p)
        , pcIndiceVraisemblance p
        ) :. ( pcEtatComplementaire p
             , ingId ))

-- ---------------------------------------------------------------------------
-- Insertion index (R64) — aplatissement de la hiérarchie

-- | Insère les valeurs d'index d'une 'MesureR64' dans @elec_index_values@.
-- Aplatit la hiérarchie @contexte → grandeur → calendrier → classeTemporelle → valeur@.
-- Utilise @INSERT@ simple (pas d'idempotence car plusieurs relevés peuvent coexister).
insertIndexValues :: Connection -> IngestionId -> MesureR64 -> IO ()
insertIndexValues conn ingId m =
  mapM_ insertCtx (mr64Contextes m)
  where
    insertCtx ctx =
      mapM_ (insertGrandeur ctx) (ctx64Grandeurs ctx)
    insertGrandeur ctx g = do
      mapM_ (insertCal ctx g) (gr64Calendriers g)
      case gr64CadranTotalisateur g of
        Nothing  -> return ()
        Just cdt -> mapM_ (insertTot ctx g cdt) (ctotValeurs cdt)
    insertCal ctx g cal =
      mapM_ (insertClasse ctx g cal) (calClassesTemporelles cal)
    insertClasse ctx g cal ct =
      mapM_ (insertVal ctx g cal ct False) (ctValeurs ct)
    insertTot ctx g cdt vi =
      execute conn
        "INSERT INTO elec_index_values \
        \ (etape_metier, contexte_releve, type_releve, motif_releve, \
        \  grandeur_metier, grandeur_physique, unite, \
        \  id_calendrier, libelle_grille, id_classe_temporelle, \
        \  libelle_classe_temp, code_cadran, is_totalisateur, \
        \  horodate, valeur, iv, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        (( etapeMetierToText (ctx64EtapeMetier ctx)
        , contexteReleveToText (ctx64ContexteReleve ctx)
        , typeReleveToText (ctx64TypeReleve ctx)
        , ctx64MotifReleve ctx
        , grandeurMetierToText (gr64GrandeurMetier g)
        , gr64GrandeurPhysique g
        , gr64Unite g
        , Nothing :: Maybe Text
        , Nothing :: Maybe Text
        , Nothing :: Maybe Text
        ) :. ( Nothing :: Maybe Text
             , Just (ctotCodeCadran cdt)
             , 1 :: Int
             , fmtUTC (viHorodate vi)
             , viValeur vi
             , viIndiceVraisemblance vi
             , ingId ))
    insertVal ctx g cal ct _isTot vi =
      execute conn
        "INSERT INTO elec_index_values \
        \ (etape_metier, contexte_releve, type_releve, motif_releve, \
        \  grandeur_metier, grandeur_physique, unite, \
        \  id_calendrier, libelle_grille, id_classe_temporelle, \
        \  libelle_classe_temp, code_cadran, is_totalisateur, \
        \  horodate, valeur, iv, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        (( etapeMetierToText (ctx64EtapeMetier ctx)
        , contexteReleveToText (ctx64ContexteReleve ctx)
        , typeReleveToText (ctx64TypeReleve ctx)
        , ctx64MotifReleve ctx
        , grandeurMetierToText (gr64GrandeurMetier g)
        , gr64GrandeurPhysique g
        , gr64Unite g
        , Just (calIdCalendrier cal)
        , Just (calLibelleGrille cal)
        , ctIdClasseTemporelle ct
        ) :. ( ctLibelleClasseTemporelle ct
             , ctCodeCadran ct
             , 0 :: Int
             , fmtUTC (viHorodate vi)
             , viValeur vi
             , viIndiceVraisemblance vi
             , ingId ))

-- ---------------------------------------------------------------------------
-- Insertion énergies quotidiennes (R65)

-- | Insère les énergies journalières d'une 'MesureR65' dans @elec_daily_energy@.
-- Utilise @INSERT OR REPLACE@ : idempotent sur @(grandeur_metier, grandeur_physique, date_mesure)@.
insertDailyEnergy :: Connection -> IngestionId -> MesureR65 -> IO ()
insertDailyEnergy conn ingId m =
  mapM_ insertGrandeur (mr65Grandeurs m)
  where
    insertGrandeur g =
      mapM_ (insertPoint g) (gr65Points g)
    insertPoint g p =
      execute conn
        "INSERT OR REPLACE INTO elec_daily_energy \
        \ (etape_metier, grandeur_metier, grandeur_physique, unite, \
        \  mode_calcul, date_mesure, valeur, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?)"
        ( etapeMetierToText (mr65EtapeMetier m)
        , grandeurMetierToText (gr65GrandeurMetier g)
        , grandeurPhysiqueEnergieToText (gr65GrandeurPhysique g)
        , gr65Unite g
        , modeCalculToText (mr65ModeCalcul m)
        , fmtDay (peDate p)
        , peValeur p
        , ingId )

-- ---------------------------------------------------------------------------
-- Insertion Pmax quotidiennes (R66)

-- | Insère les Pmax journalières d'une 'MesureR66' dans @elec_daily_pmax@.
-- Utilise @INSERT OR REPLACE@ : idempotent sur @(grandeur_metier, grandeur_physique, horodate)@.
insertDailyPmax :: Connection -> IngestionId -> MesureR66 -> IO ()
insertDailyPmax conn ingId m =
  mapM_ insertGrandeur (mr66Grandeurs m)
  where
    insertGrandeur g =
      mapM_ (insertPoint g) (gr66Points g)
    insertPoint g p =
      execute conn
        "INSERT OR REPLACE INTO elec_daily_pmax \
        \ (etape_metier, grandeur_metier, grandeur_physique, unite, \
        \  horodate, valeur, ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?)"
        ( etapeMetierToText (mr66EtapeMetier m)
        , grandeurMetierToText (gr66GrandeurMetier g)
        , grandeurPhysiquePmaxToText (gr66GrandeurPhysique g)
        , gr66Unite g
        , fmtUTC (ppHorodate p)
        , ppValeur p
        , ingId )

-- ---------------------------------------------------------------------------
-- Insertion mesures facturantes (R67) — INSERT sans REPLACE pour conserver les statuts

-- | Insère les mesures facturantes d'une 'MesureR67' dans @elec_billing_measures@.
-- Utilise @INSERT@ simple (sans @REPLACE@) pour conserver plusieurs relevés
-- avec des statuts différents sur la même période.
insertBillingMeasures :: Connection -> IngestionId -> MesureR67 -> IO ()
insertBillingMeasures conn ingId m =
  mapM_ insertCtx (mr67Contextes m)
  where
    insertCtx ctx =
      mapM_ (insertGrandeur ctx) (ctx67Grandeurs ctx)
    insertGrandeur ctx g =
      mapM_ (insertCal ctx g) (gr67Calendriers g)
    insertCal ctx g cal =
      mapM_ (insertClasse ctx g cal) (cal67ClassesTemporelles cal)
    insertClasse ctx g cal ct =
      mapM_ (insertQuantite ctx g cal ct) (ct67Quantites ct)
    insertQuantite ctx g cal ct q =
      execute conn
        "INSERT INTO elec_billing_measures \
        \ (etape_metier, id_motif_releve, libelle_motif_releve, \
        \  grandeur_metier, grandeur_physique, unite, \
        \  code_grille, libelle_grille, code_calendrier, libelle_calendrier, \
        \  id_classe_temporelle, libelle_classe_temp, \
        \  date_creation, dbt_mesure, fin_mesure, quantite, \
        \  code_nature, libelle_nature, code_statut, libelle_statut, \
        \  ingestion_id) \
        \ VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        (( etapeMetierToText (ctx67EtapeMetier ctx)
        , ctx67IdMotifReleve ctx
        , ctx67LibelleMotifReleve ctx
        , grandeurMetierToText (gr67GrandeurMetier g)
        , gr67GrandeurPhysique g
        , gr67Unite g
        , cal67CodeGrille cal
        , cal67LibelleGrille cal
        , cal67CodeCalendrier cal
        , cal67LibelleCalendrier cal
        ) :. ( ct67IdClasseTemporelle ct
             , ct67LibelleClasseTemporelle ct
             , fmtUTC (qDateCreation q)
             , fmtDay (qDbtMesure q)
             , fmtDay (qFinMesure q)
             , qQuantite q
             , qCodeNature q
             , qLibelleNature q
             , qCodeStatut q
             , qLibelleStatut q
             ) :. Only ingId)

-- ---------------------------------------------------------------------------
-- Insertion informations techniques et contractuelles (C68)

-- | Insère les informations techniques d'un C68 dans @elec_prm_info@.
-- Le JSON brut est sérialisé en texte. Utilise @INSERT@ simple pour conserver
-- l'historique des changements contractuels (plusieurs lignes possibles par PRM).
insertPrmInfo :: Connection -> IngestionId -> UTCTime -> InfoTechniqueContractuelle -> IO ()
insertPrmInfo conn ingId dateIng itc = do
  let rawJsonText = TE.decodeUtf8 . BL.toStrict . encode $ c68RawJson itc
  execute conn
    "INSERT INTO elec_prm_info \
    \ (segment, etat_contractuel, etat_alimentation, puissance_souscrite, \
    \  domaine_tension, raw_json, date_ingestion, ingestion_id) \
    \ VALUES (?,?,?,?,?,?,?,?)"
    ( c68Segment itc
    , c68EtatContractuel itc
    , c68EtatAlimentation itc
    , c68PuissanceSouscrite itc
    , c68DomaineTension itc
    , rawJsonText
    , fmtUTC dateIng
    , ingId )
