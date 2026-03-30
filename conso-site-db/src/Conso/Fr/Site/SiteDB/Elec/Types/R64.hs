{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Types.R64
Description : Types pour les index Enedis (flux R64, R64A, R64B)

Représente le flux M023\/R6X-REC d'index compteur. La hiérarchie est :

> FluxR64 → [MesureR64] → [ContexteR64] → [GrandeurR64] → [Calendrier] → [ClasseTemporelle] → [ValeurIndex]

Chaque 'ValeurIndex' contient une valeur d'index horodatée avec son indicateur
de vraisemblance (@iv@, encodage 4 bits 0-15).

Le champ @idCalendrier@ identifie la grille tarifaire : préfixe @D@ pour un
calendrier distributeur (ex. @DI000001@) ou @F@ pour fournisseur (ex. @FC000049@).
-}
module Conso.Fr.Site.SiteDB.Elec.Types.R64 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Site.SiteDB.Elec.Types.Common
import           Conso.Fr.Site.SiteDB.Elec.Types.Header

-- | Une valeur d'index à un instant donné.
data ValeurIndex = ValeurIndex
  { viHorodate            :: UTCTime    -- ^ @d@ — Horodate de l'index
  , viValeur              :: Int        -- ^ @v@ — Valeur de l'index (Wh, VArh, …)
  , viIndiceVraisemblance :: Maybe Int  -- ^ @iv@ — Qualité (encodage 4 bits 0-15) : @null@ ou @0-15@
  } deriving (Eq, Show)

-- | Classe temporelle (tranche horaire tarifaire) avec ses valeurs d'index.
data ClasseTemporelle = ClasseTemporelle
  { ctIdClasseTemporelle      :: Maybe Text    -- ^ Identifiant de la classe (ex. @\"HPH\"@)
  , ctLibelleClasseTemporelle :: Maybe Text    -- ^ Libellé (ex. @\"Heures Pleines Hiver\"@)
  , ctCodeCadran              :: Maybe Text    -- ^ Code cadran (ex. @\"EA\"@)
  , ctValeurs                 :: [ValeurIndex] -- ^ Valeurs d'index de cette classe
  } deriving (Eq, Show)

-- | Calendrier tarifaire (grille distributeur ou fournisseur).
-- @idCalendrier@ commence par @D@ (distributeur) ou @F@ (fournisseur).
data Calendrier = Calendrier
  { calIdCalendrier       :: Text               -- ^ Identifiant du calendrier (ex. @DI000001@)
  , calLibelleCalendrier  :: Text               -- ^ Libellé du calendrier
  , calLibelleGrille      :: Text               -- ^ Libellé de la grille tarifaire
  , calClassesTemporelles :: [ClasseTemporelle] -- ^ Classes temporelles du calendrier
  } deriving (Eq, Show)

-- | Cadran totalisateur (somme toutes classes confondues).
data CadranTotalisateur = CadranTotalisateur
  { ctotCodeCadran :: Text           -- ^ Code du cadran totalisateur
  , ctotValeurs    :: [ValeurIndex]  -- ^ Valeurs totalisées
  } deriving (Eq, Show)

-- | Grandeur R64 avec ses calendriers.
-- @gr64GrandeurPhysique@ est stocké en texte brut : @DD@, @DE@, @DQ@, @EA@,
-- @ER@, @ERC@, @ERI@, @PMA@, @TF@, @PA@.
data GrandeurR64 = GrandeurR64
  { gr64GrandeurMetier     :: GrandeurMetier       -- ^ Sens (@CONS@ ou @PROD@)
  , gr64GrandeurPhysique   :: Text                 -- ^ Nature physique (texte Enedis)
  , gr64Unite              :: Text                 -- ^ Unité (ex. @\"Wh\"@)
  , gr64Calendriers        :: [Calendrier]         -- ^ Calendriers tarifaires
  , gr64CadranTotalisateur :: Maybe CadranTotalisateur -- ^ Cadran totalisateur (optionnel)
  } deriving (Eq, Show)

-- | Contexte de relevé R64 (étape métier + raison du relevé).
data ContexteR64 = ContexteR64
  { ctx64EtapeMetier    :: EtapeMetier    -- ^ Étape de traitement (@BRUT@ généralement)
  , ctx64ContexteReleve :: ContexteReleve -- ^ Contexte (@COL@, @TOP@, @FMR@, …)
  , ctx64TypeReleve     :: TypeReleve     -- ^ Type (@AQ@, @AV@, @LC@, …)
  , ctx64MotifReleve    :: Maybe Text     -- ^ Motif textuel du relevé (optionnel)
  , ctx64Grandeurs      :: [GrandeurR64]  -- ^ Grandeurs mesurées dans ce contexte
  } deriving (Eq, Show)

-- | Mesure R64 pour un PRM (un objet du tableau @mesures@).
data MesureR64 = MesureR64
  { mr64IdPrm     :: PrmId          -- ^ Identifiant du PRM
  , mr64Periode   :: Periode        -- ^ Période couverte
  , mr64Contextes :: [ContexteR64]  -- ^ Contextes de relevé (souvent 1)
  } deriving (Eq, Show)

-- | Flux R64 complet (commun aux codes R64, R64A et R64B).
data FluxR64 = FluxR64
  { r64Header    :: Header          -- ^ En-tête du fichier
  , r64Echeances :: Maybe Echeances -- ^ Bloc échéances (R6X-REC uniquement)
  , r64Mesures   :: [MesureR64]     -- ^ Une entrée par PRM
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

-- | Parse un objet @mesure@ R64 depuis le JSON.
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
