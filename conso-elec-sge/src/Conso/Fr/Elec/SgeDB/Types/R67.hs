{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.R67 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime, Day)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header

-- | Quantité facturante
data Quantite = Quantite
  { qDateCreation  :: UTCTime
  , qDbtMesure     :: Day
  , qFinMesure     :: Day
  , qQuantite      :: Int
  , qCodeNature    :: Maybe Text    -- "E", "I", "C", "R"
  , qLibelleNature :: Text
  , qCodeStatut    :: Maybe Text    -- "I", "A", "R"
  , qLibelleStatut :: Text
  } deriving (Eq, Show)

-- | Classe temporelle R67
data ClasseTemporelleR67 = ClasseTemporelleR67
  { ct67IdClasseTemporelle      :: Text
  , ct67LibelleClasseTemporelle :: Text
  , ct67Quantites               :: [Quantite]
  } deriving (Eq, Show)

-- | Calendrier R67
data CalendrierR67 = CalendrierR67
  { cal67CodeGrille         :: Maybe Text
  , cal67LibelleGrille      :: Text
  , cal67CodeCalendrier     :: Maybe Text
  , cal67LibelleCalendrier  :: Text
  , cal67ClassesTemporelles :: [ClasseTemporelleR67]
  } deriving (Eq, Show)

-- | Grandeur R67
data GrandeurR67 = GrandeurR67
  { gr67GrandeurMetier   :: GrandeurMetier
  , gr67GrandeurPhysique :: Text
  , gr67Unite            :: Text
  , gr67Calendriers      :: [CalendrierR67]
  } deriving (Eq, Show)

-- | Contexte R67
data ContexteR67 = ContexteR67
  { ctx67EtapeMetier        :: EtapeMetier
  , ctx67IdMotifReleve      :: Text
  , ctx67LibelleMotifReleve :: Text
  , ctx67Grandeurs          :: [GrandeurR67]
  } deriving (Eq, Show)

-- | Mesure R67 pour un PRM
data MesureR67 = MesureR67
  { mr67IdPrm     :: PrmId
  , mr67Periode   :: Periode
  , mr67Contextes :: [ContexteR67]
  } deriving (Eq, Show)

-- | Flux R67 complet
data FluxR67 = FluxR67
  { r67Header  :: Header
  , r67Mesures :: [MesureR67]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON Quantite where
  parseJSON = withObject "Quantite" $ \o -> do
    dc  <- o .:  "dateCreation" >>= parseDateTimeText
    dbt <- o .:  "dbtMesure"    >>= parseDayText
    fin <- o .:  "finMesure"    >>= parseDayText
    q   <- o .:  "quantite"
    cn  <- o .:? "codeNature"
    ln  <- o .:  "libelleNature"
    cs  <- o .:? "codeStatut"
    ls  <- o .:  "libelleStatut"
    pure $ Quantite dc dbt fin q cn ln cs ls

instance FromJSON ClasseTemporelleR67 where
  parseJSON = withObject "ClasseTemporelleR67" $ \o ->
    ClasseTemporelleR67
      <$> o .: "idClasseTemporelle"
      <*> o .: "libelleClasseTemporelle"
      <*> o .: "quantite"

instance FromJSON CalendrierR67 where
  parseJSON = withObject "CalendrierR67" $ \o ->
    CalendrierR67
      <$> o .:? "codeGrille"
      <*> o .:  "libelleGrille"
      <*> o .:? "codeCalendrier"
      <*> o .:  "libelleCalendrier"
      <*> o .:  "classeTemporelle"

instance FromJSON GrandeurR67 where
  parseJSON = withObject "GrandeurR67" $ \o ->
    GrandeurR67
      <$> o .: "grandeurMetier"
      <*> o .: "grandeurPhysique"
      <*> o .: "unite"
      <*> o .: "calendrier"

instance FromJSON ContexteR67 where
  parseJSON = withObject "ContexteR67" $ \o ->
    ContexteR67
      <$> o .: "etapeMetier"
      <*> o .: "idMotifReleve"
      <*> o .: "libelleMotifReleve"
      <*> o .: "grandeur"

parseMesureR67 :: Value -> Parser MesureR67
parseMesureR67 = withObject "MesureR67" $ \o ->
  MesureR67
    <$> (PrmId <$> o .: "idPrm")
    <*> parsePeriode o
    <*> o .: "contexte"

instance FromJSON FluxR67 where
  parseJSON = withObject "FluxR67" $ \o -> do
    header  <- parseHeaderM023 o
    mes     <- o .: "mesures"
    mesures <- mapM parseMesureR67 mes
    pure $ FluxR67 header mesures
