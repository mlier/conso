{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Query
Description : Requêtes SQLite pour toutes les tables SgeDB

Définit les types de résultat aplatis (@CurveRow@, @IndexRow@, …) et les
fonctions de requête par période, grandeur et étape métier.

Les filtres @Maybe Text@ sont tous optionnels : passer 'Nothing' revient
à ne pas filtrer sur ce critère. Les bornes temporelles sont des chaînes
ISO 8601 (@YYYY-MM-DDTHH:MM:SS@ ou @YYYY-MM-DD@).
-}
module Conso.Fr.Elec.SiteDB.Storage.Query
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
  , derniereHorodateCourbe
  , derniereDateEnergie
  , derniereDatePmax
  ) where

import           Database.SQLite.Simple
import           Data.Text                      (Text)
import qualified Data.Text                      as T

-- ---------------------------------------------------------------------------
-- Types de résultat aplatis (lignes de la base)

-- | Ligne de résultat d'une requête sur @elec_curve_points@.
data CurveRow = CurveRow
  { crEtapeMetier      :: Text       -- ^ @etape_metier@ — @\"BRUT\"@ ou @\"BEST\"@
  , crGrandeurMetier   :: Text       -- ^ @grandeur_metier@ — @\"CONS\"@ ou @\"PROD\"@
  , crGrandeurPhysique :: Text       -- ^ @grandeur_physique@ — @\"PA\"@, @\"PRI\"@, …
  , crUnite            :: Text       -- ^ @unite@ — ex. @\"W\"@
  , crHorodate         :: Text       -- ^ @horodate@ — ISO 8601
  , crValeur           :: Text       -- ^ @valeur@ — valeur de puissance (chaîne)
  , crPas              :: Text       -- ^ @pas@ — @\"PT5M\"@..@\"PT60M\"@
  , crNature           :: Text       -- ^ @nature@ — @\"B\"@, @\"C\"@, @\"E\"@, …
  , crTypeCompletion   :: Maybe Text -- ^ @type_completion@ — présent si @etapeMetier = BEST@
  , crIv               :: Maybe Int  -- ^ @iv@ — indicateur de vraisemblance (0-2)
  , crEc               :: Maybe Int  -- ^ @ec@ — état complémentaire (si @iv = 2@)
  } deriving (Eq, Show)

instance FromRow CurveRow where
  fromRow = CurveRow <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field <*> field <*> field

-- | Ligne de résultat d'une requête sur @elec_index_values@.
data IndexRow = IndexRow
  { irEtapeMetier      :: Text       -- ^ @etape_metier@
  , irContexteReleve   :: Text       -- ^ @contexte_releve@ — @\"COL\"@, @\"TOP\"@, …
  , irTypeReleve       :: Text       -- ^ @type_releve@ — @\"AQ\"@, @\"LC\"@, …
  , irMotifReleve      :: Maybe Text -- ^ @motif_releve@
  , irGrandeurMetier   :: Text       -- ^ @grandeur_metier@
  , irGrandeurPhysique :: Text       -- ^ @grandeur_physique@ — @\"EA\"@, @\"PMA\"@, …
  , irUnite            :: Text       -- ^ @unite@
  , irIdCalendrier     :: Maybe Text -- ^ @id_calendrier@ (ex. @DI000001@)
  , irLibelleGrille    :: Maybe Text -- ^ @libelle_grille@
  , irIdClasse         :: Maybe Text -- ^ @id_classe_temporelle@
  , irLibelleClasse    :: Maybe Text -- ^ @libelle_classe_temp@
  , irCodeCadran       :: Maybe Text -- ^ @code_cadran@
  , irIsTotalisateur   :: Int        -- ^ @is_totalisateur@ — @1@ si cadran totalisateur, @0@ sinon
  , irHorodate         :: Text       -- ^ @horodate@ — ISO 8601
  , irValeur           :: Int        -- ^ @valeur@ — valeur entière de l'index
  , irIv               :: Maybe Int  -- ^ @iv@ — indicateur vraisemblance (4 bits 0-15)
  } deriving (Eq, Show)

instance FromRow IndexRow where
  fromRow = IndexRow <$> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field
                     <*> field <*> field <*> field <*> field

-- | Ligne de résultat d'une requête sur @elec_daily_energy@.
data EnergyRow = EnergyRow
  { erEtapeMetier      :: Text -- ^ @etape_metier@
  , erGrandeurMetier   :: Text -- ^ @grandeur_metier@
  , erGrandeurPhysique :: Text -- ^ @grandeur_physique@ — @\"EA\"@, @\"ERI\"@, @\"ERC\"@
  , erUnite            :: Text -- ^ @unite@ — ex. @\"Wh\"@
  , erModeCalcul       :: Text -- ^ @mode_calcul@ — @\"DIFF_INDEX\"@ ou @\"INTEG_COURBE\"@
  , erDateMesure       :: Text -- ^ @date_mesure@ — @YYYY-MM-DD@
  , erValeur           :: Text -- ^ @valeur@ — énergie de la journée (chaîne)
  } deriving (Eq, Show)

instance FromRow EnergyRow where
  fromRow = EnergyRow <$> field <*> field <*> field <*> field
                      <*> field <*> field <*> field

-- | Ligne de résultat d'une requête sur @elec_daily_pmax@.
data PmaxRow = PmaxRow
  { pmEtapeMetier      :: Text -- ^ @etape_metier@
  , pmGrandeurMetier   :: Text -- ^ @grandeur_metier@
  , pmGrandeurPhysique :: Text -- ^ @grandeur_physique@ — @\"PMA\"@, @\"PMA1\"@..@\"PMA3\"@
  , pmUnite            :: Text -- ^ @unite@ — toujours @\"VA\"@
  , pmHorodate         :: Text -- ^ @horodate@ — instant exact de la Pmax (ISO 8601)
  , pmValeur           :: Text -- ^ @valeur@ — valeur de la Pmax (chaîne, en VA)
  } deriving (Eq, Show)

instance FromRow PmaxRow where
  fromRow = PmaxRow <$> field <*> field <*> field
                    <*> field <*> field <*> field

-- | Ligne de résultat d'une requête sur @elec_billing_measures@.
data BillingRow = BillingRow
  { brEtapeMetier       :: Text       -- ^ @etape_metier@ — @\"FACT\"@
  , brIdMotifReleve     :: Text       -- ^ @id_motif_releve@ — code du motif de relevé
  , brGrandeurMetier    :: Text       -- ^ @grandeur_metier@
  , brGrandeurPhysique  :: Text       -- ^ @grandeur_physique@
  , brUnite             :: Text       -- ^ @unite@
  , brCodeGrille        :: Maybe Text -- ^ @code_grille@
  , brLibelleGrille     :: Text       -- ^ @libelle_grille@ — ex. @\"HC-HP\"@
  , brCodeCalendrier    :: Maybe Text -- ^ @code_calendrier@
  , brLibelleCalendrier :: Text       -- ^ @libelle_calendrier@
  , brIdClasse          :: Text       -- ^ @id_classe_temporelle@ — ex. @\"HPH\"@
  , brLibelleClasse     :: Text       -- ^ @libelle_classe_temp@
  , brDateCreation      :: Text       -- ^ @date_creation@
  , brDbtMesure         :: Text       -- ^ @dbt_mesure@ — début de la période (@YYYY-MM-DD@)
  , brFinMesure         :: Text       -- ^ @fin_mesure@ — fin de la période (@YYYY-MM-DD@)
  , brQuantite          :: Int        -- ^ @quantite@ — valeur entière facturée
  , brCodeNature        :: Maybe Text -- ^ @code_nature@ — @E@, @I@, @C@, @R@
  , brLibelleNature     :: Text       -- ^ @libelle_nature@
  , brCodeStatut        :: Maybe Text -- ^ @code_statut@ — @I@, @A@, @R@
  , brLibelleStatut     :: Text       -- ^ @libelle_statut@
  } deriving (Eq, Show)

instance FromRow BillingRow where
  fromRow = BillingRow <$> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field <*> field
                       <*> field <*> field <*> field

-- | Ligne de résultat d'une requête sur @elec_prm_info@.
data PrmInfoRow = PrmInfoRow
  { piId               :: Int        -- ^ @id@ — clé primaire auto-incrémentée
  , piSegment          :: Maybe Text -- ^ @segment@ — @\"C5\"@, @\"P4\"@, …
  , piEtatContractuel  :: Maybe Text -- ^ @etat_contractuel@
  , piEtatAlimentation :: Maybe Text -- ^ @etat_alimentation@
  , piPuissance        :: Maybe Text -- ^ @puissance_souscrite@ (kVA)
  , piDomaineTension   :: Maybe Text -- ^ @domaine_tension@ — @\"BT\"@, @\"HTA\"@, @\"HTB\"@
  , piRawJson          :: Text       -- ^ @raw_json@ — JSON C68 complet sérialisé
  , piDateIngestion    :: Text       -- ^ @date_ingestion@
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
             \ FROM elec_curve_points \
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
             \ FROM elec_index_values \
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
             \ FROM elec_daily_energy \
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
             \ FROM elec_daily_pmax \
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
             \ FROM elec_billing_measures \
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
    \ FROM elec_prm_info ORDER BY id DESC LIMIT 1"
  return $ case rows of
    []    -> Nothing
    (r:_) -> Just r

-- ---------------------------------------------------------------------------
-- Dernières dates disponibles par table

derniereHorodateCourbe :: Connection -> IO (Maybe Text)
derniereHorodateCourbe conn = scalarQuery conn
  "SELECT SUBSTR(MAX(horodate),1,10) FROM elec_curve_points"

derniereDateEnergie :: Connection -> IO (Maybe Text)
derniereDateEnergie conn = scalarQuery conn
  "SELECT MAX(date_mesure) FROM elec_daily_energy"

derniereDatePmax :: Connection -> IO (Maybe Text)
derniereDatePmax conn = scalarQuery conn
  "SELECT MAX(date(horodate)) FROM elec_daily_pmax"

scalarQuery :: Connection -> Query -> IO (Maybe Text)
scalarQuery conn q = do
  rows <- query_ conn q :: IO [Only (Maybe Text)]
  return $ case rows of
    [Only mv] -> mv
    _         -> Nothing

-- ---------------------------------------------------------------------------
-- Helper : construction de clauses WHERE optionnelles

whereClause :: [(Text, Maybe Text)] -> Text
whereClause filters =
  let active = [(col, val) | (col, Just val) <- filters]
  in case active of
    [] -> ""
    xs -> " AND " <> T.intercalate " AND "
            [col <> " = '" <> val <> "'" | (col, val) <- xs]
