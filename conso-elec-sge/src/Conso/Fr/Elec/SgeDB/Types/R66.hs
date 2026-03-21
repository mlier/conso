{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.R66 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header

-- | Point Pmax quotidien (horodate exacte de la Pmax)
data PointPmax = PointPmax
  { ppValeur   :: Text     -- "v"
  , ppHorodate :: UTCTime  -- "d" : horodate fonctionnelle exacte
  } deriving (Eq, Show)

-- | Grandeur R66
data GrandeurR66 = GrandeurR66
  { gr66GrandeurMetier   :: GrandeurMetier
  , gr66GrandeurPhysique :: GrandeurPhysiquePmax
  , gr66Unite            :: Text     -- "VA"
  , gr66Points           :: [PointPmax]
  } deriving (Eq, Show)

-- | Mesure R66 pour un PRM
data MesureR66 = MesureR66
  { mr66IdPrm       :: PrmId
  , mr66EtapeMetier :: EtapeMetier
  , mr66Periode     :: Periode
  , mr66ModeCalcul  :: ModeCalcul
  , mr66Pas         :: Pas         -- P1D
  , mr66Grandeurs   :: [GrandeurR66]
  } deriving (Eq, Show)

-- | Flux R66 complet (R66, R66B)
data FluxR66 = FluxR66
  { r66Header    :: Header
  , r66Echeances :: Maybe Echeances
  , r66Mesures   :: [MesureR66]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON GrandeurPhysiquePmax where
  parseJSON = withText "GrandeurPhysiquePmax" $ \t -> case t of
    "PMA"  -> pure GP_PMA_MONO
    "PMA1" -> pure GP_PMA1
    "PMA2" -> pure GP_PMA2
    "PMA3" -> pure GP_PMA3
    _      -> pure GP_PMA_MONO   -- fallback

instance FromJSON PointPmax where
  parseJSON = withObject "PointPmax" $ \o -> do
    v <- o .: "v"
    d <- o .: "d" >>= parseDateTimeText
    pure $ PointPmax v d

instance FromJSON GrandeurR66 where
  parseJSON = withObject "GrandeurR66" $ \o ->
    GrandeurR66
      <$> o .: "grandeurMetier"
      <*> o .: "grandeurPhysique"
      <*> o .: "unite"
      <*> o .: "points"

parseMesureR66 :: Value -> Parser MesureR66
parseMesureR66 = withObject "MesureR66" $ \o ->
  MesureR66
    <$> (PrmId <$> o .: "idPrm")
    <*> o .: "etapeMetier"
    <*> parsePeriode o
    <*> o .: "modeCalcul"
    <*> o .: "pas"
    <*> o .: "grandeur"

instance FromJSON FluxR66 where
  parseJSON = withObject "FluxR66" $ \o -> do
    hdr   <- o .: "header"
    isPub <- (hdr :: Object) .:? "idPublication" :: Parser (Maybe Text)
    header  <- case isPub of
      Just _  -> parseHeaderREC o
      Nothing -> parseHeaderM023 o
    ech     <- parseEcheances o
    mes     <- o .: "mesures"
    mesures <- mapM parseMesureR66 mes
    pure $ FluxR66 header ech mesures
