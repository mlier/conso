{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.R63 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header

-- | Un point de courbe de charge
data PointCourbe = PointCourbe
  { pcValeur              :: Text           -- "v" : valeur arrondie (string)
  , pcHorodate            :: UTCTime        -- "d"
  , pcPas                 :: Pas            -- "p"
  , pcNature              :: NaturePoint    -- "n"
  , pcTypeCompletion      :: Maybe TypeCompletion  -- "tc" (optionnel)
  , pcIndiceVraisemblance :: Maybe Int      -- "iv" (null ou 0-2)
  , pcEtatComplementaire  :: Maybe Int      -- "ec" (null ou 0,1,5-11)
  } deriving (Eq, Show)

-- | Grandeur R63 avec ses points
data GrandeurR63 = GrandeurR63
  { gr63GrandeurMetier   :: GrandeurMetier
  , gr63GrandeurPhysique :: GrandeurPhysiqueR63
  , gr63Unite            :: Text
  , gr63Points           :: [PointCourbe]
  } deriving (Eq, Show)

-- | Mesure R63 pour un PRM
data MesureR63 = MesureR63
  { mr63IdPrm       :: PrmId
  , mr63EtapeMetier :: EtapeMetier
  , mr63Periode     :: Periode
  , mr63ModeCalcul  :: ModeCalcul
  , mr63Grandeurs   :: [GrandeurR63]
  } deriving (Eq, Show)

-- | Flux R63 complet (R63, R63A, R63B)
data FluxR63 = FluxR63
  { r63Header    :: Header
  , r63Echeances :: Maybe Echeances
  , r63Mesures   :: [MesureR63]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON PointCourbe where
  parseJSON = withObject "PointCourbe" $ \o -> do
    v   <- o .:  "v"
    d   <- o .:  "d" >>= parseDateTimeText
    p   <- o .:  "p"
    n   <- o .:  "n"
    tc  <- o .:? "tc"
    iv  <- o .:? "iv"
    ec  <- o .:? "ec"
    pure $ PointCourbe v d p n tc iv ec

instance FromJSON GrandeurR63 where
  parseJSON = withObject "GrandeurR63" $ \o ->
    GrandeurR63
      <$> o .: "grandeurMetier"
      <*> o .: "grandeurPhysique"
      <*> o .: "unite"
      <*> o .: "points"

parseMesureR63 :: Value -> Parser MesureR63
parseMesureR63 = withObject "MesureR63" $ \o ->
  MesureR63
    <$> (PrmId <$> o .: "idPrm")
    <*> o .: "etapeMetier"
    <*> parsePeriode o
    <*> o .: "modeCalcul"
    <*> o .: "grandeur"

instance FromJSON FluxR63 where
  parseJSON = withObject "FluxR63" $ \o -> do
    hdr   <- o .: "header"
    isPub <- (hdr :: Object) .:? "idPublication" :: Parser (Maybe Text)
    header  <- case isPub of
      Just _  -> parseHeaderREC o
      Nothing -> parseHeaderM023 o
    ech     <- parseEcheances o
    mes     <- o .: "mesures"
    mesures <- mapM parseMesureR63 mes
    pure $ FluxR63 header ech mesures
