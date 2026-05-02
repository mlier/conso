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
  ) where

import Data.Text (Text)

-- | Enregistrement de consommation gaz pour stockage SQLite.
data GazConso = GazConso
  -- Consommation
  { gcEnergie             :: Maybe Double
  , gcVolumeBrut          :: Maybe Double
  , gcVolumeConverti      :: Maybe Double
  , gcConversion          :: Maybe Double
  , gcPta                 :: Maybe Double
  , gcPcs                 :: Maybe Double
  , gcFlagRetourZero      :: Maybe Bool
  , gcTypeQualif          :: Maybe Text
  , gcSensFlux            :: Maybe Text
  , gcStatutConso         :: Maybe Text
  , gcTypeConso           :: Maybe Text
  , gcJourneeGaziere      :: Maybe Text
  -- Relevé début (NOT NULL — clé unique)
  , gcDebut               :: Text
  , gcDebutRaison         :: Maybe Text
  , gcDebutLibelleRaison  :: Maybe Text
  , gcDebutQualite        :: Maybe Text
  , gcDebutStatut         :: Maybe Text
  , gcDebutIndexBrut      :: Maybe Double
  , gcDebutIndexConverti  :: Maybe Double
  -- Relevé fin (NOT NULL — clé unique)
  , gcFin                 :: Text
  , gcFinRaison           :: Maybe Text
  , gcFinLibelleRaison    :: Maybe Text
  , gcFinQualite          :: Maybe Text
  , gcFinStatut           :: Maybe Text
  , gcFinIndexBrut        :: Maybe Double
  , gcFinIndexConverti    :: Maybe Double
  } deriving (Show)

-- | Enregistrement d'injection gaz pour stockage SQLite.
data GazInjection = GazInjection
  -- Injection
  { giEnergie            :: Maybe Double
  , giVolumeBrut         :: Maybe Double
  , giVolumeConverti     :: Maybe Double
  , giConversion         :: Maybe Double
  , giPta                :: Maybe Double
  , giPcs                :: Maybe Double
  , giFlagRetourZero     :: Maybe Bool
  , giTypeQualif         :: Maybe Text
  , giSensFlux           :: Maybe Text
  , giStatut             :: Maybe Text
  , giTypeInjection      :: Maybe Text
  , giJourneeGaziere     :: Maybe Text
  -- Relevé début (NOT NULL — clé unique)
  , giDebut              :: Text
  , giDebutRaison        :: Maybe Text
  , giDebutLibelleRaison :: Maybe Text
  , giDebutQualite       :: Maybe Text
  , giDebutStatut        :: Maybe Text
  , giDebutIndexBrut     :: Maybe Double
  , giDebutIndexConverti :: Maybe Double
  -- Relevé fin (NOT NULL — clé unique)
  , giFin                :: Text
  , giFinRaison          :: Maybe Text
  , giFinLibelleRaison   :: Maybe Text
  , giFinQualite         :: Maybe Text
  , giFinStatut          :: Maybe Text
  , giFinIndexBrut       :: Maybe Double
  , giFinIndexConverti   :: Maybe Double
  } deriving (Show)

-- | Informations contractuelles GRDF pour stockage SQLite (série temporelle).
-- Une ligne est insérée uniquement quand les données changent.
data GazInfosContractuelles = GazInfosContractuelles
  { icDateMes                   :: Maybe Text -- ^ Date de mise en service
  , icTarifAcheminement         :: Maybe Text -- ^ Tarif d'acheminement (T1, T2, …)
  , icDatePublication           :: Maybe Text -- ^ Date de publication des données
  , icConsoJournalierePlafond   :: Maybe Text -- ^ Consommation journalière plafond
  , icCarActuelle               :: Maybe Text -- ^ CAR actuelle
  , icCarFuture                 :: Maybe Text -- ^ CAR future
  , icCja                       :: Maybe Text -- ^ CJA
  , icCjaJournaliere            :: Maybe Text -- ^ CJA journalière
  , icCjaMensuelle              :: Maybe Text -- ^ CJA mensuelle
  , icProfilTypeActuel          :: Maybe Text -- ^ Profil type actuel (ex. P012)
  , icProfilTypeFutur           :: Maybe Text -- ^ Profil type futur
  , icDateDebutProfilTypeActuel :: Maybe Text -- ^ Début validité profil actuel
  , icDateFinProfilTypeActuel   :: Maybe Text -- ^ Fin validité profil actuel
  , icModulationAssiette        :: Maybe Text -- ^ Modulation assiette
  , icModulationN1              :: Maybe Text -- ^ Modulation N-1
  , icModulationN2              :: Maybe Text -- ^ Modulation N-2
  , icModulationN3              :: Maybe Text -- ^ Modulation N-3
  } deriving (Show, Eq)

-- | Informations techniques GRDF pour stockage SQLite (série temporelle).
-- Une ligne est insérée uniquement quand les données changent.
data GazInfosTechniques = GazInfosTechniques
  -- Situation compteur
  { itNumeroRue                    :: Maybe Text
  , itNomRue                       :: Maybe Text
  , itComplementAdresse            :: Maybe Text
  , itCodePostal                   :: Maybe Text
  , itCommune                      :: Maybe Text
  -- Caractéristiques compteur
  , itClientSensibleMig            :: Maybe Text
  , itCodeCalibre                  :: Maybe Text
  , itCodeDebit                    :: Maybe Text
  , itCodeDebitNormalise           :: Maybe Text
  , itFrequence                    :: Maybe Text
  , itMatriculeCompteur            :: Maybe Text
  , itPressionLivraison            :: Maybe Text
  -- PITD
  , itIdentifiantPitd              :: Maybe Text
  , itLibellePitd                  :: Maybe Text
  -- Régime de propriété
  , itRegimeProprieteCompteur      :: Maybe Text
  , itRegimeProprieteConvertisseur :: Maybe Text
  , itRegimeProprieteEnregistreur  :: Maybe Text
  , itRegimeProprietePoste         :: Maybe Text
  } deriving (Show, Eq)
