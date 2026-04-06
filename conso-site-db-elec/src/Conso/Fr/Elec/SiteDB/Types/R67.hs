{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Types.R67
Description : Types pour les mesures facturantes Enedis (flux R67)

Représente le flux M023 ponctuel des mesures facturantes. La hiérarchie est :

> FluxR67 → [MesureR67] → [ContexteR67] → [GrandeurR67] → [CalendrierR67] → [ClasseTemporelleR67] → [Quantite]

Chaque 'Quantite' représente une mesure sur une période de facturation avec
son motif de relevé, sa grille tarifaire, sa classe temporelle, sa valeur
entière et ses codes nature\/statut.

Ce flux est ponctuel M023 uniquement (pas de variante REC).
-}
module Conso.Fr.Elec.SiteDB.Types.R67 where

import           Data.Text                                (Text)
import           Data.Time                                (UTCTime, Day)
import           Data.Aeson
import           Data.Aeson.Types                         (Parser)
import           Conso.Fr.Elec.SiteDB.Types.Common
import           Conso.Fr.Elec.SiteDB.Types.Header

-- | Quantité facturante (feuille de la hiérarchie R67).
data Quantite = Quantite
  { qDateCreation  :: UTCTime     -- ^ Date de création de l'enregistrement
  , qDbtMesure     :: Day         -- ^ Début de la période de facturation (@YYYY-MM-DD@)
  , qFinMesure     :: Day         -- ^ Fin de la période de facturation (@YYYY-MM-DD@)
  , qQuantite      :: Int         -- ^ Valeur mesurée (entière, ex. Wh)
  , qCodeNature    :: Maybe Text  -- ^ Code nature (@E@=estimé, @I@=initial, @C@=corrigé, @R@=réel)
  , qLibelleNature :: Text        -- ^ Libellé de la nature
  , qCodeStatut    :: Maybe Text  -- ^ Code statut (@I@=initial, @A@=annulé, @R@=remplacé)
  , qLibelleStatut :: Text        -- ^ Libellé du statut
  } deriving (Eq, Show)

-- | Classe temporelle R67 (tranche tarifaire, ex. HPH, HCH).
data ClasseTemporelleR67 = ClasseTemporelleR67
  { ct67IdClasseTemporelle      :: Text       -- ^ Identifiant de la classe (ex. @\"HPH\"@)
  , ct67LibelleClasseTemporelle :: Text       -- ^ Libellé (ex. @\"Heures Pleines Hiver\"@)
  , ct67Quantites               :: [Quantite] -- ^ Quantités facturantes de cette classe
  } deriving (Eq, Show)

-- | Calendrier R67 (grille tarifaire associée à une grandeur).
data CalendrierR67 = CalendrierR67
  { cal67CodeGrille         :: Maybe Text              -- ^ Code de la grille tarifaire (optionnel)
  , cal67LibelleGrille      :: Text                    -- ^ Libellé de la grille (ex. @\"HC-HP\"@)
  , cal67CodeCalendrier     :: Maybe Text              -- ^ Code du calendrier tarifaire
  , cal67LibelleCalendrier  :: Text                    -- ^ Libellé du calendrier
  , cal67ClassesTemporelles :: [ClasseTemporelleR67]   -- ^ Classes temporelles du calendrier
  } deriving (Eq, Show)

-- | Grandeur R67 avec ses calendriers tarifaires.
data GrandeurR67 = GrandeurR67
  { gr67GrandeurMetier   :: GrandeurMetier  -- ^ Sens (@CONS@ ou @PROD@)
  , gr67GrandeurPhysique :: Text            -- ^ Nature physique (texte brut Enedis)
  , gr67Unite            :: Text            -- ^ Unité (ex. @\"Wh\"@)
  , gr67Calendriers      :: [CalendrierR67] -- ^ Calendriers tarifaires
  } deriving (Eq, Show)

-- | Contexte de relevé R67 (étape métier + motif de relevé).
data ContexteR67 = ContexteR67
  { ctx67EtapeMetier        :: EtapeMetier   -- ^ Étape (@FACT@ généralement)
  , ctx67IdMotifReleve      :: Text          -- ^ Code du motif de relevé (ex. relève périodique)
  , ctx67LibelleMotifReleve :: Text          -- ^ Libellé du motif de relevé
  , ctx67Grandeurs          :: [GrandeurR67] -- ^ Grandeurs facturantes
  } deriving (Eq, Show)

-- | Mesure R67 pour un PRM (un objet du tableau @mesures@).
data MesureR67 = MesureR67
  { mr67IdPrm     :: PrmId          -- ^ Identifiant du PRM
  , mr67Periode   :: Periode        -- ^ Période couverte par le relevé facturant
  , mr67Contextes :: [ContexteR67]  -- ^ Contextes de relevé
  } deriving (Eq, Show)

-- | Flux R67 complet (ponctuel M023 uniquement).
data FluxR67 = FluxR67
  { r67Header  :: Header       -- ^ En-tête du fichier
  , r67Mesures :: [MesureR67]  -- ^ Une entrée par PRM
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

-- | Parse un objet @mesure@ R67 depuis le JSON.
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
