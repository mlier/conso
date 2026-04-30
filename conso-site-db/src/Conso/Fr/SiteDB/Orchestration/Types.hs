{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.SiteDB.Orchestration.Types
  ( Accord(..)
  , TypeFlux(..)
  , typeFluxToStr
  , Rattachement(..)
  , InscriptionPrmParams(..)
  , InscriptionPceParams(..)
  , InscriptionResult(..)
  , SgeAbonnement(..)
  , VerifAdresse(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)
import Conso.Fr.SiteDB.Types (SiteId)

data Accord
  = AccordNom Text
  | AccordDenomination Text
  deriving (Show)

data TypeFlux = CDC | IDX | ENERGIE | PMAX
  deriving (Show, Eq)

typeFluxToStr :: TypeFlux -> String
typeFluxToStr CDC     = "CDC"
typeFluxToStr IDX     = "IDX"
typeFluxToStr ENERGIE = "ENERGIE"
typeFluxToStr PMAX    = "PMAX"

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
  { ipePce          :: Text
  , ipeCodePostal   :: Text
  , ipeEmail        :: Maybe Text
  , ipeAccord       :: Accord
  , ipeRattachement :: Rattachement
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
