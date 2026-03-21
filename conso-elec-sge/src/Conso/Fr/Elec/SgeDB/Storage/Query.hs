{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Storage.Query
  ( CurveRow(..)
  , IndexRow(..)
  , EnergyRow(..)
  , PmaxRow(..)
  , BillingRow(..)
  , PrmInfoRow(..)
  , queryCurvePoints
  , queryIndexValues
  , queryDailyEnergy
  , queryDailyPmax
  , queryBillingMeasures
  , queryPrmInfo
  ) where

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Maybe                     (maybeToList)
import           Conso.Fr.Elec.SgeDB.Types.Common

-- ---------------------------------------------------------------------------
-- Types de résultat aplatis (lignes de la base)

data CurveRow = CurveRow
  { crEtapeMetier      :: Text
  , crGrandeurMetier   :: Text
  , crGrandeurPhysique :: Text
  , crUnite            :: Text
  , crHorodate         :: Text
  , crValeur           :: Text
  , crPas              :: Text
  , crNature           :: Text
  , crTypeCompletion   :: Maybe Text
  , crIv               :: Maybe Int
  , crEc               :: Maybe Int
  } deriving (Eq, Show)

instance FromRow CurveRow where
  fromRow = CurveRow <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field <*> field <*> field

data IndexRow = IndexRow
  { irEtapeMetier      :: Text
  , irContexteReleve   :: Text
  , irTypeReleve       :: Text
  , irMotifReleve      :: Maybe Text
  , irGrandeurMetier   :: Text
  , irGrandeurPhysique :: Text
  , irUnite            :: Text
  , irIdCalendrier     :: Maybe Text
  , irLibelleGrille    :: Maybe Text
  , irIdClasse         :: Maybe Text
  , irLibelleClasse    :: Maybe Text
  , irCodeCadran       :: Maybe Text
  , irIsTotalisateur   :: Int
  , irHorodate         :: Text
  , irValeur           :: Int
  , irIv               :: Maybe Int
  } deriving (Eq, Show)

instance FromRow IndexRow where
  fromRow = IndexRow <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field

data EnergyRow = EnergyRow
  { erEtapeMetier      :: Text
  , erGrandeurMetier   :: Text
  , erGrandeurPhysique :: Text
  , erUnite            :: Text
  , erModeCalcul       :: Text
  , erDateMesure       :: Text
  , erValeur           :: Text
  } deriving (Eq, Show)

instance FromRow EnergyRow where
  fromRow = EnergyRow <$> field <*> field <*> field <*> field
                      <*> field <*> field <*> field

data PmaxRow = PmaxRow
  { pmEtapeMetier      :: Text
  , pmGrandeurMetier   :: Text
  , pmGrandeurPhysique :: Text
  , pmUnite            :: Text
  , pmHorodate         :: Text
  , pmValeur           :: Text
  } deriving (Eq, Show)

instance FromRow PmaxRow where
  fromRow = PmaxRow <$> field <*> field <*> field
                    <*> field <*> field <*> field

data BillingRow = BillingRow
  { brEtapeMetier     :: Text
  , brIdMotifReleve   :: Text
  , brGrandeurMetier  :: Text
  , brGrandeurPhysique:: Text
  , brUnite           :: Text
  , brCodeGrille      :: Maybe Text
  , brLibelleGrille   :: Text
  , brCodeCalendrier  :: Maybe Text
  , brLibelleCalendrier :: Text
  , brIdClasse        :: Text
  , brLibelleClasse   :: Text
  , brDateCreation    :: Text
  , brDbtMesure       :: Text
  , brFinMesure       :: Text
  , brQuantite        :: Int
  , brCodeNature      :: Maybe Text
  , brLibelleNature   :: Text
  , brCodeStatut      :: Maybe Text
  , brLibelleStatut   :: Text
  } deriving (Eq, Show)

instance FromRow BillingRow where
  fromRow = BillingRow <$> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field

data PrmInfoRow = PrmInfoRow
  { piId               :: Int
  , piSegment          :: Maybe Text
  , piEtatContractuel  :: Maybe Text
  , piEtatAlimentation :: Maybe Text
  , piPuissance        :: Maybe Text
  , piDomaineTension   :: Maybe Text
  , piRawJson          :: Text
  , piDateIngestion    :: Text
  } deriving (Eq, Show)

instance FromRow PrmInfoRow where
  fromRow = PrmInfoRow <$> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field

-- ---------------------------------------------------------------------------
-- Requêtes

-- | Courbes de charge sur une période
queryCurvePoints
  :: Connection
  -> Maybe Text      -- etape_metier (BRUT, BEST)
  -> Maybe Text      -- grandeur_metier (CONS, PROD)
  -> Maybe Text      -- grandeur_physique (PA, PRI, ...)
  -> Text            -- horodate début (ISO 8601)
  -> Text            -- horodate fin   (ISO 8601)
  -> IO [CurveRow]
queryCurvePoints conn mEtape mGm mGp deb fin =
  query conn
    (Query $ "SELECT etape_metier, grandeur_metier, grandeur_physique, unite, \
             \  horodate, valeur, pas, nature, type_completion, iv, ec \
             \ FROM curve_points \
             \ WHERE horodate >= ? AND horodate <= ?"
             <> whereClause [("etape_metier", mEtape), ("grandeur_metier", mGm)
                            ,("grandeur_physique", mGp)]
             <> " ORDER BY grandeur_metier, grandeur_physique, horodate")
    (deb, fin)

-- | Index sur une période
queryIndexValues
  :: Connection
  -> Maybe Text   -- contexte_releve
  -> Maybe Text   -- grandeur_physique
  -> Text -> Text -- période
  -> IO [IndexRow]
queryIndexValues conn mCtx mGp deb fin =
  query conn
    (Query $ "SELECT etape_metier, contexte_releve, type_releve, motif_releve, \
             \  grandeur_metier, grandeur_physique, unite, \
             \  id_calendrier, libelle_grille, id_classe_temporelle, \
             \  libelle_classe_temp, code_cadran, is_totalisateur, \
             \  horodate, valeur, iv \
             \ FROM index_values \
             \ WHERE horodate >= ? AND horodate <= ?"
             <> whereClause [("contexte_releve", mCtx), ("grandeur_physique", mGp)]
             <> " ORDER BY contexte_releve, grandeur_physique, horodate")
    (deb, fin)

-- | Énergies quotidiennes sur une période de dates
queryDailyEnergy
  :: Connection
  -> Maybe Text   -- grandeur_metier
  -> Text -> Text -- date début/fin (YYYY-MM-DD)
  -> IO [EnergyRow]
queryDailyEnergy conn mGm deb fin =
  query conn
    (Query $ "SELECT etape_metier, grandeur_metier, grandeur_physique, unite, \
             \  mode_calcul, date_mesure, valeur \
             \ FROM daily_energy \
             \ WHERE date_mesure >= ? AND date_mesure <= ?"
             <> whereClause [("grandeur_metier", mGm)]
             <> " ORDER BY grandeur_metier, grandeur_physique, date_mesure")
    (deb, fin)

-- | Pmax quotidiennes sur une période
queryDailyPmax
  :: Connection
  -> Maybe Text   -- grandeur_metier
  -> Text -> Text -- période
  -> IO [PmaxRow]
queryDailyPmax conn mGm deb fin =
  query conn
    (Query $ "SELECT etape_metier, grandeur_metier, grandeur_physique, unite, \
             \  horodate, valeur \
             \ FROM daily_pmax \
             \ WHERE horodate >= ? AND horodate <= ?"
             <> whereClause [("grandeur_metier", mGm)]
             <> " ORDER BY grandeur_metier, grandeur_physique, horodate")
    (deb, fin)

-- | Mesures facturantes sur une période de dates
queryBillingMeasures
  :: Connection
  -> Maybe Text   -- grandeur_metier
  -> Text -> Text -- date début/fin (YYYY-MM-DD)
  -> IO [BillingRow]
queryBillingMeasures conn mGm deb fin =
  query conn
    (Query $ "SELECT etape_metier, id_motif_releve, grandeur_metier, \
             \  grandeur_physique, unite, code_grille, libelle_grille, \
             \  code_calendrier, libelle_calendrier, id_classe_temporelle, \
             \  libelle_classe_temp, date_creation, dbt_mesure, fin_mesure, \
             \  quantite, code_nature, libelle_nature, code_statut, libelle_statut \
             \ FROM billing_measures \
             \ WHERE dbt_mesure <= ? AND fin_mesure >= ?"
             <> whereClause [("grandeur_metier", mGm)]
             <> " ORDER BY grandeur_metier, grandeur_physique, dbt_mesure")
    (fin, deb)

-- | Informations techniques courantes (dernière ligne ingérée)
queryPrmInfo :: Connection -> IO (Maybe PrmInfoRow)
queryPrmInfo conn = do
  rows <- query_ conn
    "SELECT id, segment, etat_contractuel, etat_alimentation, \
    \  puissance_souscrite, domaine_tension, raw_json, date_ingestion \
    \ FROM prm_info ORDER BY id DESC LIMIT 1"
  return $ case rows of
    []    -> Nothing
    (r:_) -> Just r

-- | Historique complet des informations techniques
queryPrmInfoHistory :: Connection -> IO [PrmInfoRow]
queryPrmInfoHistory conn =
  query_ conn
    "SELECT id, segment, etat_contractuel, etat_alimentation, \
    \  puissance_souscrite, domaine_tension, raw_json, date_ingestion \
    \ FROM prm_info ORDER BY id ASC"

-- ---------------------------------------------------------------------------
-- Helper : construction de clauses WHERE optionnelles

whereClause :: [(Text, Maybe Text)] -> Text
whereClause filters =
  let active = [(col, val) | (col, Just val) <- filters]
  in case active of
    [] -> ""
    xs -> " AND " <> T.intercalate " AND "
            [col <> " = '" <> val <> "'" | (col, val) <- xs]
