{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Types.R65
Description : Types pour les énergies quotidiennes Enedis (flux R65)

Représente le flux M023 ponctuel des énergies quotidiennes. La hiérarchie est :

> FluxR65 → [MesureR65] → [GrandeurR65] → [PointEnergie]

Chaque 'PointEnergie' représente l'énergie d'une journée entière (pas @P1D@).
Le mode de calcul varie selon le segment :

  * @DIFF.INDEX@ — Différence d'index à minuit (C5\/P4 Linky)
  * @INTEG.COURBE@ — Intégrale de la courbe de charge (C1-C4)
-}
module Conso.Fr.Elec.SiteDB.Types.R65 where

import           Data.Text                                (Text)
import           Data.Time                                (Day)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SiteDB.Types.Common
import           Conso.Fr.Elec.SiteDB.Types.Header

-- | Point d'énergie quotidienne (une journée calendaire complète).
data PointEnergie = PointEnergie
  { peValeur :: Text -- ^ @v@ — Énergie de la journée (chaîne, en Wh ou VArh selon grandeur)
  , peDate   :: Day  -- ^ @d@ — Date fonctionnelle (@YYYY-MM-DD@)
  } deriving (Eq, Show)

-- | Grandeur R65 avec ses points journaliers.
data GrandeurR65 = GrandeurR65
  { gr65GrandeurMetier   :: GrandeurMetier          -- ^ Sens (@CONS@ ou @PROD@)
  , gr65GrandeurPhysique :: GrandeurPhysiqueEnergie -- ^ Type d'énergie (@EA@, @ERI@, @ERC@)
  , gr65Unite            :: Text                    -- ^ Unité (ex. @\"Wh\"@)
  , gr65Points           :: [PointEnergie]          -- ^ Points journaliers ordonnés par date
  } deriving (Eq, Show)

-- | Mesure R65 pour un PRM (un objet du tableau @mesures@).
data MesureR65 = MesureR65
  { mr65IdPrm       :: PrmId          -- ^ Identifiant du PRM
  , mr65EtapeMetier :: EtapeMetier    -- ^ Étape (@BRUT@ ou @BEST@)
  , mr65Periode     :: Periode        -- ^ Période couverte
  , mr65TypeValeur  :: Text           -- ^ Type de valeur (ex. @\"GLOBALE\"@)
  , mr65ModeCalcul  :: ModeCalcul     -- ^ @DIFF_INDEX@ (C5\/P4) ou @INTEG_COURBE@ (C1-C4)
  , mr65Pas         :: Pas            -- ^ Toujours @P1D@ (journalier)
  , mr65Grandeurs   :: [GrandeurR65]  -- ^ Grandeurs mesurées
  } deriving (Eq, Show)

-- | Flux R65 complet (ponctuel M023 uniquement, pas de variante REC).
data FluxR65 = FluxR65
  { r65Header  :: Header       -- ^ En-tête du fichier
  , r65Mesures :: [MesureR65]  -- ^ Une entrée par PRM
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

-- | Parse un objet @mesure@ R65 depuis le JSON.
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
