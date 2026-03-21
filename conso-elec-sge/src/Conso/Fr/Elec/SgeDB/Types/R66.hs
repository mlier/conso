{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Types.R66
Description : Types pour les puissances maximales quotidiennes Enedis (flux R66, R66B)

Représente le flux M023\/R6X-REC des Pmax quotidiennes. La hiérarchie est :

> FluxR66 → [MesureR66] → [GrandeurR66] → [PointPmax]

Chaque 'PointPmax' contient la valeur de puissance apparente maximale atteinte
dans la journée et l'horodate exacte à laquelle elle a été mesurée.

Ce flux est disponible uniquement pour les compteurs Linky (segments C5\/P4).
L'unité est toujours @VA@ (VoltAmpère, puissance apparente).
-}
module Conso.Fr.Elec.SgeDB.Types.R66 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header

-- | Point Pmax quotidien : valeur maximale sur la journée et instant exact.
data PointPmax = PointPmax
  { ppValeur   :: Text    -- ^ @v@ — Valeur de la Pmax (en VA, chaîne)
  , ppHorodate :: UTCTime -- ^ @d@ — Horodate fonctionnelle exacte de la Pmax
  } deriving (Eq, Show)

-- | Grandeur R66 avec ses points Pmax journaliers.
data GrandeurR66 = GrandeurR66
  { gr66GrandeurMetier   :: GrandeurMetier      -- ^ Sens (@CONS@ ou @PROD@)
  , gr66GrandeurPhysique :: GrandeurPhysiquePmax -- ^ Phase mesurée (@PMA@, @PMA1@, @PMA2@, @PMA3@)
  , gr66Unite            :: Text                -- ^ Toujours @\"VA\"@ (puissance apparente)
  , gr66Points           :: [PointPmax]         -- ^ Un point par journée
  } deriving (Eq, Show)

-- | Mesure R66 pour un PRM (un objet du tableau @mesures@).
data MesureR66 = MesureR66
  { mr66IdPrm       :: PrmId         -- ^ Identifiant du PRM
  , mr66EtapeMetier :: EtapeMetier   -- ^ Étape (@BRUT@)
  , mr66Periode     :: Periode       -- ^ Période couverte
  , mr66ModeCalcul  :: ModeCalcul    -- ^ Mode de calcul
  , mr66Pas         :: Pas           -- ^ Toujours @P1D@ (journalier)
  , mr66Grandeurs   :: [GrandeurR66] -- ^ Grandeurs mesurées (une par phase)
  } deriving (Eq, Show)

-- | Flux R66 complet (commun aux codes R66 et R66B).
-- 'r66Echeances' est présent uniquement pour R66B (service récurrent).
data FluxR66 = FluxR66
  { r66Header    :: Header          -- ^ En-tête du fichier
  , r66Echeances :: Maybe Echeances -- ^ Bloc échéances (R6X-REC uniquement)
  , r66Mesures   :: [MesureR66]     -- ^ Une entrée par PRM
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

-- | Parse un objet @mesure@ R66 depuis le JSON.
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
