{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.SiteDB.Types
Description : Types SQLite pour le stockage des données GRDF ADICT

Types optimisés pour l'insertion et la requête SQLite des données gaz.
Distincts des types API ADICT de @conso-gaz-adict@ qui gèrent la
sérialisation JSON des appels REST.
-}
module Conso.Fr.Gaz.SiteDB.Types
  ( GazConso(..)
  , GazInjection(..)
  , GazInfosContractuelles(..)
  , GazInfosTechniques(..)
  , TypeDonnee(..)
  , PeriodeGaz(..)
  , typeDonneeToText
  , periodeGazToText
  ) where

import Data.Text (Text)

-- | Type de donnée (qualité de la restitution).
data TypeDonnee
  = TDPubliee     -- ^ Donnée publiée définitive
  | TDInformative -- ^ Donnée informative (provisoire)
  deriving (Eq, Ord, Show)

typeDonneeToText :: TypeDonnee -> Text
typeDonneeToText TDPubliee     = "PUBLIEE"
typeDonneeToText TDInformative = "INFORMATIVE"

-- | Granularité temporelle de la période.
data PeriodeGaz
  = PJournalier -- ^ Données journalières
  | PMensuel    -- ^ Données mensuelles
  deriving (Eq, Ord, Show)

periodeGazToText :: PeriodeGaz -> Text
periodeGazToText PJournalier = "J"
periodeGazToText PMensuel    = "M"

-- | Enregistrement de consommation gaz pour stockage SQLite.
data GazConso = GazConso
  { gcDateDebut        :: Text          -- ^ Date début (YYYY-MM-DD)
  , gcDateFin          :: Text          -- ^ Date fin (YYYY-MM-DD)
  , gcPeriode          :: PeriodeGaz    -- ^ Granularité (J ou M)
  , gcTypeDonnee       :: TypeDonnee    -- ^ Qualité (PUBLIEE/INFORMATIVE)
  , gcEnergie          :: Maybe Double  -- ^ Énergie en kWh
  , gcVolumeBrut       :: Maybe Double  -- ^ Volume brut en m³
  , gcVolumeConverti   :: Maybe Double  -- ^ Volume converti en m³
  , gcCoeffConversion  :: Maybe Double  -- ^ Coefficient de conversion PCS
  , gcCoeffPta         :: Maybe Double  -- ^ Coefficient PTA
  , gcRawJson          :: Text          -- ^ JSON brut de la ligne NDJSON
  } deriving (Show)

-- | Enregistrement d'injection gaz pour stockage SQLite.
data GazInjection = GazInjection
  { giDateDebut      :: Text         -- ^ Date début (YYYY-MM-DD)
  , giDateFin        :: Text         -- ^ Date fin (YYYY-MM-DD)
  , giPeriode        :: PeriodeGaz   -- ^ Granularité (J ou M)
  , giTypeDonnee     :: TypeDonnee   -- ^ Qualité
  , giEnergie        :: Maybe Double -- ^ Énergie injectée en kWh
  , giVolumeBrut     :: Maybe Double -- ^ Volume brut en m³
  , giVolumeConverti :: Maybe Double -- ^ Volume converti en m³
  , giRawJson        :: Text         -- ^ JSON brut de la ligne NDJSON
  } deriving (Show)

-- | Informations contractuelles GRDF pour stockage SQLite.
data GazInfosContractuelles = GazInfosContractuelles
  { icDateDebut       :: Maybe Text -- ^ Date de début du contrat
  , icDateFin         :: Maybe Text -- ^ Date de fin du contrat
  , icSegmentClient   :: Maybe Text -- ^ Segment client (T1, T2, …)
  , icNumCompteur     :: Maybe Text -- ^ Numéro de compteur
  , icTarif           :: Maybe Text -- ^ Tarif applicable
  , icRawJson         :: Text       -- ^ JSON brut complet
  } deriving (Show, Eq)

-- | Informations techniques GRDF pour stockage SQLite.
data GazInfosTechniques = GazInfosTechniques
  { itTypeCompteur    :: Maybe Text -- ^ Type de compteur
  , itPression        :: Maybe Text -- ^ Domaine de pression (BP, MP, …)
  , itDateReleve      :: Maybe Text -- ^ Date du dernier relevé
  , itEtatCompteur    :: Maybe Text -- ^ État du compteur
  , itRawJson         :: Text       -- ^ JSON brut complet
  } deriving (Show, Eq)
