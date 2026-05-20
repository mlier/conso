{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Types.Nass
  ( MotifFinNass(..)
  , motifFinFromCode
  , motifFinToInt
  , NassService(..)
  , NassSegment(..)
  , NassServiceSouscrit(..)
  , FluxNassJson(..)
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Aeson
import           Text.Read              (readMaybe)

import           Conso.Fr.SiteDB.Orchestration.Types (TypeFlux(..), typeFluxFromStr)
import           Conso.Fr.Elec.SiteDB.Types.Common
  ( PrmId(..), TypeService(..), EtatService(..)
  , typeServiceFromText, etatServiceFromText )

-- ---------------------------------------------------------------------------
-- Motif de fin de service

data MotifFinNass
  = MotifFinNull                  -- ^ 0 — balise vide
  | MotifFinExpiration            -- ^ 1
  | MotifFinArretBeneficiaire     -- ^ 2
  | MotifFinLeveeOpposition       -- ^ 3
  | MotifFinArretClient           -- ^ 4
  | MotifFinRenouvellement        -- ^ 5
  | MotifFinRemplacement          -- ^ 6
  | MotifFinChangementFournisseur -- ^ 7
  | MotifFinResiliationPoint      -- ^ 8
  | MotifFinOppositionCdC         -- ^ 11
  | MotifFinChangementOffre       -- ^ 12
  | MotifFinArretEnedis           -- ^ 13
  | MotifFinInconnu Text          -- ^ catch-all pour codes futurs
  deriving (Show, Eq)

motifFinFromCode :: Text -> MotifFinNass
motifFinFromCode "0"  = MotifFinNull
motifFinFromCode "1"  = MotifFinExpiration
motifFinFromCode "2"  = MotifFinArretBeneficiaire
motifFinFromCode "3"  = MotifFinLeveeOpposition
motifFinFromCode "4"  = MotifFinArretClient
motifFinFromCode "5"  = MotifFinRenouvellement
motifFinFromCode "6"  = MotifFinRemplacement
motifFinFromCode "7"  = MotifFinChangementFournisseur
motifFinFromCode "8"  = MotifFinResiliationPoint
motifFinFromCode "11" = MotifFinOppositionCdC
motifFinFromCode "12" = MotifFinChangementOffre
motifFinFromCode "13" = MotifFinArretEnedis
motifFinFromCode c    = MotifFinInconnu c

-- | Code Enedis natif pour stockage SQLite (INTEGER, pas fromEnum).
motifFinToInt :: MotifFinNass -> Maybe Int
motifFinToInt MotifFinNull                  = Just 0
motifFinToInt MotifFinExpiration            = Just 1
motifFinToInt MotifFinArretBeneficiaire     = Just 2
motifFinToInt MotifFinLeveeOpposition       = Just 3
motifFinToInt MotifFinArretClient           = Just 4
motifFinToInt MotifFinRenouvellement        = Just 5
motifFinToInt MotifFinRemplacement          = Just 6
motifFinToInt MotifFinChangementFournisseur = Just 7
motifFinToInt MotifFinResiliationPoint      = Just 8
motifFinToInt MotifFinOppositionCdC         = Just 11
motifFinToInt MotifFinChangementOffre       = Just 12
motifFinToInt MotifFinArretEnedis           = Just 13
motifFinToInt (MotifFinInconnu t)           = readMaybe (T.unpack t)

-- ---------------------------------------------------------------------------
-- Types du flux

data NassServiceSouscrit = NassServiceSouscrit
  { nassTypeServiceSouscrit :: TypeFlux      -- ^ CDC | IDX | PMAX | ENERGIE | ITC
  , nassTypeService         :: TypeService   -- ^ Soutirage | Injection
  , nassEtatService         :: EtatService   -- ^ Termine | ...
  , nassDateDebut           :: Text
  , nassDateFin             :: Text
  , nassMotifFin            :: MotifFinNass
  , nassMotifFinLibelle     :: Maybe Text    -- ^ libelle brut (absent pour code 0)
  } deriving (Show)

data NassSegment = NassSegment
  { nassSegmentLabel     :: Text
  , nassServiceSouscrits :: [NassServiceSouscrit]
  } deriving (Show)

data NassService = NassService
  { nassIdPrm    :: PrmId
  , nassSegments :: [NassSegment]
  } deriving (Show)

-- | Enveloppe racine du JSON NASS ({ header, services }).
newtype FluxNassJson = FluxNassJson { nassJsonServices :: [NassService] }

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON NassServiceSouscrit where
  parseJSON = withObject "NassServiceSouscrit" $ \o -> do
    tssStr <- o .: "typeServiceSouscrit"
    tss <- case typeFluxFromStr tssStr of
      Just tf -> pure tf
      Nothing -> fail $ "typeServiceSouscrit inconnu: " ++ tssStr
    tsStr <- o .: "typeService"
    ts <- case typeServiceFromText tsStr of
      Just v  -> pure v
      Nothing -> fail $ "typeService inconnu: " ++ T.unpack tsStr
    esStr <- o .: "etatService"
    codeStr <- o .: "motifFinCode"
    pure NassServiceSouscrit
      <*> pure tss
      <*> pure ts
      <*> pure (etatServiceFromText esStr)
      <*> o .: "dateDebut"
      <*> o .: "dateFin"
      <*> pure (motifFinFromCode codeStr)
      <*> o .:? "motifFinLibelle"

instance FromJSON NassSegment where
  parseJSON = withObject "NassSegment" $ \o ->
    NassSegment
      <$> o .: "segment"
      <*> o .: "serviceSouscrits"

instance FromJSON NassService where
  parseJSON = withObject "NassService" $ \o ->
    NassService
      <$> (PrmId <$> o .: "idPrm")
      <*> o .: "segments"


instance FromJSON FluxNassJson where
  parseJSON = withObject "FluxNassJson" $ \o ->
    FluxNassJson <$> o .: "services"
