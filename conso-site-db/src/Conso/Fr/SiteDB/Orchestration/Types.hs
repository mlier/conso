{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Types
  ( Accord(..)
  , TypeFlux(..)
  , typeFluxToStr
  , typeFluxFromStr
  , Rattachement(..)
  , InscriptionPrmParams(..)
  , InscriptionPceParams(..)
  , InscriptionResult(..)
  , SgeAbonnement(..)
  , VerifAdresse(..)
  , GetCodePostal
  ) where

import Data.Text (Text)
import Data.UUID (UUID)
import Conso.Fr.SiteDB.Types (SiteId)

-- | Abstraction fournie par une extension pour obtenir le code postal d'un point de livraison.
-- Règle : toute extension gérant un point avec adresse physique doit exporter une valeur de ce type.
type GetCodePostal = Text -> IO (Either String Text)

data Accord
  = AccordNom Text
  | AccordDenomination Text
  deriving (Show)

data TypeFlux = CDC | IDX | ENERGIE | PMAX | ITC
  deriving (Show, Eq)

typeFluxToStr :: TypeFlux -> String
typeFluxToStr CDC     = "CDC"
typeFluxToStr IDX     = "IDX"
typeFluxToStr ENERGIE = "ENERGIE"
typeFluxToStr PMAX    = "PMAX"
typeFluxToStr ITC     = "ITC"

typeFluxFromStr :: String -> Maybe TypeFlux
typeFluxFromStr "CDC"     = Just CDC
typeFluxFromStr "IDX"     = Just IDX
typeFluxFromStr "ENERGIE" = Just ENERGIE
typeFluxFromStr "PMAX"    = Just PMAX
typeFluxFromStr "ITC"     = Just ITC
typeFluxFromStr _         = Nothing

data Rattachement
  = Standalone
  | ParPce Text Bool
  | ParPrm Text Bool
  | ParSite UUID Bool
  deriving (Show)

data InscriptionPrmParams = InscriptionPrmParams
  { ippPrm          :: Text
  , ippTypes        :: [TypeFlux]
  , ippAccord       :: Accord
  , ippRattachement :: Rattachement
  } deriving (Show)

data InscriptionPceParams = InscriptionPceParams
  { ipePce              :: Text
  , ipeCodePostal       :: Text
  , ipeEmail            :: Maybe Text
  , ipeAccord           :: Accord
  , ipeRattachement     :: Rattachement
  , ipeAvecInjections   :: Bool
  } deriving (Show)

data SgeAbonnement = SgeNouveau | SgeRenouvele deriving (Show)

data InscriptionResult = InscriptionResult
  { irSiteId      :: SiteId
  , irCreated     :: Bool
  , irSgeResults  :: [(TypeFlux, Either (String, String) SgeAbonnement)]
  , irAdictResult :: Maybe (Either String Text)
  } deriving (Show)

data VerifAdresse
  = CodePostauxIdentiques
  | Mismatch Text Text
  | VerifImpossible String
  deriving (Show)
