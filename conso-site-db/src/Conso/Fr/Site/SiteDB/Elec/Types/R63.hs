{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Types.R63
Description : Types pour les courbes de charge Enedis (flux R63, R63A, R63B)

Représente le flux M023\/R6X-REC de courbes de charge. La hiérarchie est :

> FluxR63 → [MesureR63] → [GrandeurR63] → [PointCourbe]

Chaque 'PointCourbe' contient une valeur de puissance horodatée avec son pas,
sa nature et ses indicateurs de qualité (@iv@, @ec@, @tc@).

Couvre les segments C1-C5 et P1-P4 ; les champs @iv@\/@ec@\/@tc@ ne sont
renseignés que pour les compteurs Linky (C5\/P4).
-}
module Conso.Fr.Site.SiteDB.Elec.Types.R63 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Site.SiteDB.Elec.Types.Common
import           Conso.Fr.Site.SiteDB.Elec.Types.Header

-- | Un point de courbe de charge (objet dans le tableau @points@ du JSON).
data PointCourbe = PointCourbe
  { pcValeur              :: Text                 -- ^ @v@ — Valeur de puissance arrondie (chaîne)
  , pcHorodate            :: UTCTime              -- ^ @d@ — Horodate fonctionnelle du point
  , pcPas                 :: Pas                  -- ^ @p@ — Durée du pas (@PT5M@..@PT60M@)
  , pcNature              :: NaturePoint          -- ^ @n@ — Nature du point (@B@, @C@, @E@, …)
  , pcTypeCompletion      :: Maybe TypeCompletion -- ^ @tc@ — Type de complétion (si @etapeMetier = BEST@)
  , pcIndiceVraisemblance :: Maybe Int            -- ^ @iv@ — Qualité : @null@\/@0@ OK, @1@ surveiller, @2@ avec EC (C5\/P4)
  , pcEtatComplementaire  :: Maybe Int            -- ^ @ec@ — État complémentaire si @iv = 2@ (C5\/P4)
  } deriving (Eq, Show)

-- | Grandeur physique R63 avec l'ensemble de ses points de courbe.
data GrandeurR63 = GrandeurR63
  { gr63GrandeurMetier   :: GrandeurMetier      -- ^ Sens (@CONS@ ou @PROD@)
  , gr63GrandeurPhysique :: GrandeurPhysiqueR63 -- ^ Nature physique (@PA@, @PRI@, @PRC@, @E@)
  , gr63Unite            :: Text                -- ^ Unité de mesure (ex. @\"W\"@)
  , gr63Points           :: [PointCourbe]       -- ^ Points de mesure ordonnés par horodate
  } deriving (Eq, Show)

-- | Mesure R63 pour un PRM donné (un objet du tableau @mesures@).
data MesureR63 = MesureR63
  { mr63IdPrm       :: PrmId          -- ^ Identifiant du PRM (14 chiffres)
  , mr63EtapeMetier :: EtapeMetier    -- ^ @BRUT@ ou @BEST@
  , mr63Periode     :: Periode        -- ^ Période couverte par le flux
  , mr63ModeCalcul  :: ModeCalcul     -- ^ Mode de calcul des valeurs
  , mr63Grandeurs   :: [GrandeurR63]  -- ^ Grandeurs mesurées (souvent 1 seule)
  } deriving (Eq, Show)

-- | Flux R63 complet (commun aux codes R63, R63A et R63B).
-- 'r63Echeances' est présent uniquement pour R63A et R63B (services récurrents).
data FluxR63 = FluxR63
  { r63Header    :: Header          -- ^ En-tête du fichier
  , r63Echeances :: Maybe Echeances -- ^ Bloc échéances (R6X-REC uniquement)
  , r63Mesures   :: [MesureR63]     -- ^ Une entrée par PRM dans le fichier
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

-- | Parse un objet @mesure@ R63 depuis le JSON (utilisé par l'instance 'FromJSON' de 'FluxR63').
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
