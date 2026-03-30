{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Types.Header
Description : En-tête commun et code flux des fichiers M023 Enedis

Définit le 'CodeFlux' (11 constructeurs couvrant les flux R63..R67, C68,
et leurs variantes récurrentes), le 'Header' commun à tous les flux R6X,
et le bloc 'Echeances' présent uniquement dans les services récurrents (R6X-REC).

Chaque fichier M023 contient un objet @header@ racine avec notamment :

  * @codeFlux@ — identifie le type de données
  * @modePublication@ — @\"P\"@ (ponctuel M023) ou @\"Q\"@\/@\"H\"@\/@\"M\"@ (récurrent)
  * @publicationCrp@ — @\"oui\"@ pour M023, @\"non\"@ pour REC
-}
module Conso.Fr.Site.SiteDB.Elec.Types.Header where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time                       (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                (Parser)
import           Conso.Fr.Site.SiteDB.Elec.Types.Common (parseDateTimeText, ModePublication(..))

-- | Code flux identifiant le type de fichier M023.
data CodeFlux
  = CF_R63  -- ^ Courbes de charge — M023 ponctuel (@R63@)
  | CF_R64  -- ^ Index (relevés compteur) — M023 ponctuel (@R64@)
  | CF_R65  -- ^ Énergies quotidiennes — M023 ponctuel (@R65@)
  | CF_R66  -- ^ Puissances maximales (Pmax) — M023 ponctuel (@R66@)
  | CF_R67  -- ^ Mesures facturantes — M023 ponctuel (@R67@)
  | CF_R63A -- ^ Courbes de charge récurrentes, variante A (@R63A@)
  | CF_R63B -- ^ Courbes de charge récurrentes, variante B (@R63B@)
  | CF_R64A -- ^ Index récurrents, variante A (@R64A@)
  | CF_R64B -- ^ Index récurrents, variante B (@R64B@)
  | CF_R66B -- ^ Pmax récurrentes (@R66B@)
  | CF_C68  -- ^ Informations Techniques et Contractuelles (@C68@)
  deriving (Eq, Ord, Show)

-- | Parse un 'CodeFlux' depuis sa représentation textuelle Enedis (ex. @\"R63\"@, @\"C68\"@).
-- Retourne 'Nothing' si le code n'est pas reconnu.
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

-- | Convertit un 'CodeFlux' en sa représentation textuelle Enedis.
codeFluxToText :: CodeFlux -> Text
codeFluxToText CF_R63  = "R63";  codeFluxToText CF_R64  = "R64"
codeFluxToText CF_R65  = "R65";  codeFluxToText CF_R66  = "R66"
codeFluxToText CF_R67  = "R67";  codeFluxToText CF_R63A = "R63A"
codeFluxToText CF_R63B = "R63B"; codeFluxToText CF_R64A = "R64A"
codeFluxToText CF_R64B = "R64B"; codeFluxToText CF_R66B = "R66B"
codeFluxToText CF_C68  = "C68"

-- | En-tête commun à tous les flux R6X (objet @header@ du JSON racine).
data Header = Header
  { hSiDemandeur      :: Text        -- ^ SIRET du demandeur (11 chiffres)
  , hTypeDestinataire :: Text        -- ^ Type du destinataire
  , hIdDestinataire   :: Text        -- ^ Identifiant du destinataire
  , hCodeFlux         :: CodeFlux    -- ^ Type de flux (@R63@, @R64@, …)
  , hIdDemande        :: Text        -- ^ Identifiant de la demande SGE
  , hModePublication  :: ModePublication -- ^ Mode de publication (@P@\/@Q@\/@H@\/@M@)
  , hIdCanalContact   :: Maybe Text  -- ^ Canal de contact (M023 uniquement)
  , hIdPublication    :: Maybe Text  -- ^ Identifiant de publication (R6X-REC uniquement)
  , hFormat           :: Text        -- ^ Format du message (ex. @\"json\"@)
  , hPublicationCrp   :: Maybe Text  -- ^ @\"oui\"@ (M023) ou @\"non\"@ (REC)
  } deriving (Eq, Show)

-- | Type de publication récurrente (champ @type@ du bloc @echeances@).
data TypePublication
  = FilEau  -- ^ Publication quotidienne multi-guichet (@\"FIL_EAU\"@)
  | Immediat -- ^ Publication groupée hebdomadaire ou mensuelle (@\"IMMEDIAT\"@)
  deriving (Eq, Ord, Show)

-- | Bloc @echeances@ présent uniquement dans les flux R6X-REC (services récurrents).
data Echeances = Echeances
  { ecType                     :: TypePublication -- ^ Mode de déclenchement ('FilEau' ou 'Immediat')
  , ecHorodateDebutPublication :: Maybe UTCTime   -- ^ Début effectif de la publication
  , ecFrequence                :: Maybe Text       -- ^ Fréquence (ex. @\"QUOTIDIEN\"@)
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

-- | Parse le header d'un flux M023 ponctuel.
-- Attend @idCanalContact@ et utilise @modePublication = \"P\"@.
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

-- | Parse le header d'un flux R6X-REC (service récurrent).
-- Attend @idPublication@ ; @modePublication@ vaut @\"Q\"@, @\"H\"@ ou @\"M\"@.
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

-- | Parse le bloc @echeances@ optionnel du JSON racine.
-- Retourne 'Nothing' si le champ est absent (flux M023 ponctuels).
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
