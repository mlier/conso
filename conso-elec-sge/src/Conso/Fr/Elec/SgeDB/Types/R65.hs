{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.R65 where

import           Data.Text                                (Text)
import           Data.Time                                (Day)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header

-- | Point d'énergie quotidienne (date fonctionnelle uniquement)
data PointEnergie = PointEnergie
  { peValeur :: Text  -- "v"
  , peDate   :: Day   -- "d" : YYYY-MM-DD
  } deriving (Eq, Show)

-- | Grandeur R65
data GrandeurR65 = GrandeurR65
  { gr65GrandeurMetier   :: GrandeurMetier
  , gr65GrandeurPhysique :: GrandeurPhysiqueEnergie
  , gr65Unite            :: Text
  , gr65Points           :: [PointEnergie]
  } deriving (Eq, Show)

-- | Mesure R65 pour un PRM
data MesureR65 = MesureR65
  { mr65IdPrm       :: PrmId
  , mr65EtapeMetier :: EtapeMetier
  , mr65Periode     :: Periode
  , mr65TypeValeur  :: Text       -- "GLOBALE"
  , mr65ModeCalcul  :: ModeCalcul
  , mr65Pas         :: Pas        -- P1D
  , mr65Grandeurs   :: [GrandeurR65]
  } deriving (Eq, Show)

-- | Flux R65 complet (ponctuel uniquement)
data FluxR65 = FluxR65
  { r65Header  :: Header
  , r65Mesures :: [MesureR65]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON GrandeurPhysiqueEnergie where
  parseJSON = withText "GrandeurPhysiqueEnergie" $ \t -> case t of
    "EA"  -> pure GP_EA_E
    "ERI" -> pure GP_ERI_E
    "ERC" -> pure GP_ERC_E
    _     -> pure GP_EA_E   -- fallback : on stocke la valeur brute dans GrandeurR65.gr65Unite

instance FromJSON PointEnergie where
  parseJSON = withObject "PointEnergie" $ \o -> do
    v <- o .: "v"
    d <- o .: "d" >>= parseDayText
    pure $ PointEnergie v d

instance FromJSON GrandeurR65 where
  parseJSON = withObject "GrandeurR65" $ \o -> do
    gm <- o .: "grandeurMetier"
    gp <- o .: "grandeurPhysique"
    u  <- o .: "unite"
    ps <- o .: "points"
    pure $ GrandeurR65 gm gp u ps

parseMesureR65 :: Value -> Parser MesureR65
parseMesureR65 = withObject "MesureR65" $ \o ->
  MesureR65
    <$> (PrmId <$> o .: "idPrm")
    <*> o .: "etapeMetier"
    <*> parsePeriode o
    <*> o .: "typeValeur"
    <*> o .: "modeCalcul"
    <*> o .: "pas"
    <*> o .: "grandeur"

instance FromJSON FluxR65 where
  parseJSON = withObject "FluxR65" $ \o -> do
    header  <- parseHeaderM023 o
    mes     <- o .: "mesures"
    mesures <- mapM parseMesureR65 mes
    pure $ FluxR65 header mesures
