{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.R64 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header

-- | Valeur d'index
data ValeurIndex = ValeurIndex
  { viHorodate            :: UTCTime
  , viValeur              :: Int
  , viIndiceVraisemblance :: Maybe Int   -- null ou 0-15
  } deriving (Eq, Show)

-- | Classe temporelle avec ses valeurs
data ClasseTemporelle = ClasseTemporelle
  { ctIdClasseTemporelle      :: Maybe Text
  , ctLibelleClasseTemporelle :: Maybe Text
  , ctCodeCadran              :: Maybe Text
  , ctValeurs                 :: [ValeurIndex]
  } deriving (Eq, Show)

-- | Calendrier (grille fournisseur ou distributeur)
data Calendrier = Calendrier
  { calIdCalendrier       :: Text
  , calLibelleCalendrier  :: Text
  , calLibelleGrille      :: Text
  , calClassesTemporelles :: [ClasseTemporelle]
  } deriving (Eq, Show)

-- | Cadran totalisateur
data CadranTotalisateur = CadranTotalisateur
  { ctotCodeCadran :: Text
  , ctotValeurs    :: [ValeurIndex]
  } deriving (Eq, Show)

-- | Grandeur R64
data GrandeurR64 = GrandeurR64
  { gr64GrandeurMetier     :: GrandeurMetier
  , gr64GrandeurPhysique   :: Text   -- stocké en Text (DD, DE, DQ, EA, ER, ERC, ERI, PMA, TF, PA)
  , gr64Unite              :: Text
  , gr64Calendriers        :: [Calendrier]
  , gr64CadranTotalisateur :: Maybe CadranTotalisateur
  } deriving (Eq, Show)

-- | Contexte de relève R64
data ContexteR64 = ContexteR64
  { ctx64EtapeMetier    :: EtapeMetier
  , ctx64ContexteReleve :: ContexteReleve
  , ctx64TypeReleve     :: TypeReleve
  , ctx64MotifReleve    :: Maybe Text
  , ctx64Grandeurs      :: [GrandeurR64]
  } deriving (Eq, Show)

-- | Mesure R64 pour un PRM
data MesureR64 = MesureR64
  { mr64IdPrm     :: PrmId
  , mr64Periode   :: Periode
  , mr64Contextes :: [ContexteR64]
  } deriving (Eq, Show)

-- | Flux R64 complet (R64, R64A, R64B)
data FluxR64 = FluxR64
  { r64Header    :: Header
  , r64Echeances :: Maybe Echeances
  , r64Mesures   :: [MesureR64]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON ValeurIndex where
  parseJSON = withObject "ValeurIndex" $ \o -> do
    d  <- o .:  "d" >>= parseDateTimeText
    v  <- o .:  "v"
    iv <- o .:? "iv"
    pure $ ValeurIndex d v iv

instance FromJSON ClasseTemporelle where
  parseJSON = withObject "ClasseTemporelle" $ \o ->
    ClasseTemporelle
      <$> o .:? "idClasseTemporelle"
      <*> o .:? "libelleClasseTemporelle"
      <*> o .:? "codeCadran"
      <*> o .:  "valeur"

instance FromJSON Calendrier where
  parseJSON = withObject "Calendrier" $ \o ->
    Calendrier
      <$> o .: "idCalendrier"
      <*> o .: "libelleCalendrier"
      <*> o .: "libelleGrille"
      <*> o .: "classeTemporelle"

instance FromJSON CadranTotalisateur where
  parseJSON = withObject "CadranTotalisateur" $ \o ->
    CadranTotalisateur
      <$> o .: "codeCadran"
      <*> o .: "valeur"

instance FromJSON GrandeurR64 where
  parseJSON = withObject "GrandeurR64" $ \o ->
    GrandeurR64
      <$> o .:  "grandeurMetier"
      <*> o .:  "grandeurPhysique"
      <*> o .:  "unite"
      <*> o .:  "calendrier"
      <*> o .:? "cadranTotalisateur"

instance FromJSON ContexteR64 where
  parseJSON = withObject "ContexteR64" $ \o ->
    ContexteR64
      <$> o .:  "etapeMetier"
      <*> o .:  "contexteReleve"
      <*> o .:  "typeReleve"
      <*> o .:? "motifReleve"
      <*> o .:  "grandeur"

parseMesureR64 :: Value -> Parser MesureR64
parseMesureR64 = withObject "MesureR64" $ \o ->
  MesureR64
    <$> (PrmId <$> o .: "idPrm")
    <*> parsePeriode o
    <*> o .: "contexte"

instance FromJSON FluxR64 where
  parseJSON = withObject "FluxR64" $ \o -> do
    hdr   <- o .: "header"
    isPub <- (hdr :: Object) .:? "idPublication" :: Parser (Maybe Text)
    header  <- case isPub of
      Just _  -> parseHeaderREC o
      Nothing -> parseHeaderM023 o
    ech     <- parseEcheances o
    mes     <- o .: "mesures"
    mesures <- mapM parseMesureR64 mes
    pure $ FluxR64 header ech mesures
