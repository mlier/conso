{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.Header where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time                       (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common (parseDateTimeText, ModePublication(..))

-- | Code flux identifiant le type de données
data CodeFlux
  = CF_R63 | CF_R64 | CF_R65 | CF_R66 | CF_R67   -- M023 (ponctuels)
  | CF_R63A | CF_R63B | CF_R64A | CF_R64B | CF_R66B  -- REC (récurrents)
  | CF_C68                                             -- Informations techniques
  deriving (Eq, Ord, Show)

codeFluxFromText :: Text -> Maybe CodeFlux
codeFluxFromText "R63"  = Just CF_R63
codeFluxFromText "R64"  = Just CF_R64
codeFluxFromText "R65"  = Just CF_R65
codeFluxFromText "R66"  = Just CF_R66
codeFluxFromText "R67"  = Just CF_R67
codeFluxFromText "R63A" = Just CF_R63A
codeFluxFromText "R63B" = Just CF_R63B
codeFluxFromText "R64A" = Just CF_R64A
codeFluxFromText "R64B" = Just CF_R64B
codeFluxFromText "R66B" = Just CF_R66B
codeFluxFromText "C68"  = Just CF_C68
codeFluxFromText _      = Nothing

codeFluxToText :: CodeFlux -> Text
codeFluxToText CF_R63  = "R63";  codeFluxToText CF_R64  = "R64"
codeFluxToText CF_R65  = "R65";  codeFluxToText CF_R66  = "R66"
codeFluxToText CF_R67  = "R67";  codeFluxToText CF_R63A = "R63A"
codeFluxToText CF_R63B = "R63B"; codeFluxToText CF_R64A = "R64A"
codeFluxToText CF_R64B = "R64B"; codeFluxToText CF_R66B = "R66B"
codeFluxToText CF_C68  = "C68"

-- | En-tête commun à tous les flux R6X
-- idCanalContact : présent pour M023 ; idPublication : présent pour REC
data Header = Header
  { hSiDemandeur     :: Text
  , hTypeDestinataire :: Text
  , hIdDestinataire   :: Text
  , hCodeFlux         :: CodeFlux
  , hIdDemande        :: Text
  , hModePublication  :: ModePublication
  , hIdCanalContact   :: Maybe Text   -- M023
  , hIdPublication    :: Maybe Text   -- REC
  , hFormat           :: Text
  , hPublicationCrp   :: Maybe Text
  } deriving (Eq, Show)

-- | Type de publication récurrente
data TypePublication = FilEau | Immediat
  deriving (Eq, Ord, Show)

-- | Bloc échéances (présent uniquement pour R6X-REC)
data Echeances = Echeances
  { ecType                     :: TypePublication
  , ecHorodateDebutPublication :: Maybe UTCTime
  , ecFrequence                :: Maybe Text
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON

instance FromJSON CodeFlux where
  parseJSON = withText "CodeFlux" $ \t ->
    case codeFluxFromText t of
      Just cf -> pure cf
      Nothing -> fail $ "CodeFlux inconnu: " ++ T.unpack t

instance FromJSON ModePublication where
  parseJSON = withText "ModePublication" $ \t -> case t of
    "P" -> pure MP_Ponctuel
    "Q" -> pure MP_Quotidien
    "H" -> pure MP_Hebdomadaire
    "M" -> pure MP_Mensuel
    _   -> fail $ "ModePublication inconnue: " ++ T.unpack t

instance FromJSON TypePublication where
  parseJSON = withText "TypePublication" $ \t -> case t of
    "FIL_EAU"  -> pure FilEau
    "IMMEDIAT" -> pure Immediat
    _          -> fail $ "TypePublication inconnue: " ++ T.unpack t

-- | Parse un header M023 (idCanalContact requis, pas idPublication)
parseHeaderM023 :: Object -> Parser Header
parseHeaderM023 o = do
  hdr <- o .: "header"
  Header
    <$> hdr .: "siDemandeur"
    <*> hdr .: "typeDestinataire"
    <*> hdr .: "idDestinataire"
    <*> hdr .: "codeFlux"
    <*> hdr .: "idDemande"
    <*> hdr .: "modePublication"
    <*> (Just <$> hdr .: "idCanalContact")
    <*> pure Nothing
    <*> hdr .: "format"
    <*> hdr .:? "publicationCrp"

-- | Parse un header REC (idPublication requis, pas idCanalContact)
parseHeaderREC :: Object -> Parser Header
parseHeaderREC o = do
  hdr <- o .: "header"
  Header
    <$> hdr .: "siDemandeur"
    <*> hdr .: "typeDestinataire"
    <*> hdr .: "idDestinataire"
    <*> hdr .: "codeFlux"
    <*> hdr .: "idDemande"
    <*> hdr .: "modePublication"
    <*> pure Nothing
    <*> (Just <$> hdr .: "idPublication")
    <*> hdr .: "format"
    <*> hdr .:? "publicationCrp"

-- | Parse le bloc écheances (optionnel dans le JSON racine)
parseEcheances :: Object -> Parser (Maybe Echeances)
parseEcheances o = do
  mEch <- o .:? "echeances"
  case mEch of
    Nothing  -> pure Nothing
    Just ech -> fmap Just $ do
      tp   <- ech .: "type"
      mHor <- ech .:? "horodateDebutPublication"
      mHorParsed <- case mHor of
        Nothing -> pure Nothing
        Just t  -> Just <$> parseDateTimeText t
      mFreq <- ech .:? "frequence"
      pure $ Echeances tp mHorParsed mFreq
