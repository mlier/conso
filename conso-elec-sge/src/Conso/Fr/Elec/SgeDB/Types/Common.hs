{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Types.Common where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime(..), Day)
import           Data.Time.Format    (parseTimeM, defaultTimeLocale)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Control.Applicative ((<|>))

-- ---------------------------------------------------------------------------
-- Types de base

-- | Identifiant PRM (14 caractères)
newtype PrmId = PrmId { unPrmId :: Text }
  deriving (Eq, Ord, Show)

-- | Grandeur métier
data GrandeurMetier = CONS | PROD
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Grandeur physique pour les courbes de charge (R63)
data GrandeurPhysiqueR63 = GP_PA | GP_PRI | GP_PRC | GP_E
  deriving (Eq, Ord, Show)

-- | Grandeur physique pour les index (R64) et mesures facturantes (R67)
data GrandeurPhysiqueIndex
  = GP_DD | GP_DE | GP_DQ | GP_EA | GP_ER | GP_ERC | GP_ERI | GP_PMA_I | GP_TF | GP_PA_IDX
  deriving (Eq, Ord, Show)

-- | Grandeur physique pour les Pmax (R66)
data GrandeurPhysiquePmax = GP_PMA_MONO | GP_PMA1 | GP_PMA2 | GP_PMA3
  deriving (Eq, Ord, Show)

-- | Grandeur physique pour les énergies quotidiennes (R65)
data GrandeurPhysiqueEnergie = GP_EA_E | GP_ERI_E | GP_ERC_E
  deriving (Eq, Ord, Show)

-- | Etape métier
data EtapeMetier = BRUT | BEST | FACT
  deriving (Eq, Ord, Show)

-- | Pas de mesure
data Pas = PT5M | PT10M | PT15M | PT30M | PT60M | P1D
  deriving (Eq, Ord, Show)

-- | Nature d'un point de courbe (R63)
data NaturePoint = N_B | N_C | N_R | N_D | N_S | N_T | N_F | N_G | N_H | N_E | N_P
  deriving (Eq, Ord, Show)

-- | Type de complétion R63
data TypeCompletion
  = TC_F | TC_G | TC_N | TC_H | TC_I | TC_O | TC_J | TC_K | TC_L | TC_C | TC_A | TC_M
  deriving (Eq, Ord, Show)

-- | Contexte de relève (R64)
data ContexteReleve = COL | TOP | FMR | CRD | CRI | RHF
  deriving (Eq, Ord, Show)

-- | Type de relève (R64)
data TypeReleve = TR_AQ | TR_AV | TR_AS | TR_AP | TR_LC | TR_RM | TR_RC
  deriving (Eq, Ord, Show)

-- | Mode de calcul
data ModeCalcul = MESURE | DIFF_INDEX | INTEG_COURBE
  deriving (Eq, Ord, Show)

-- | Mode de publication
data ModePublication = MP_Ponctuel | MP_Quotidien | MP_Hebdomadaire | MP_Mensuel
  deriving (Eq, Ord, Show)

-- | Période fonctionnelle
data Periode = Periode
  { periodeDebut :: UTCTime
  , periodeFin   :: UTCTime
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Instances FromJSON pour les types communs

instance FromJSON GrandeurMetier where
  parseJSON = withText "GrandeurMetier" $ \t -> case t of
    "CONS" -> pure CONS; "PROD" -> pure PROD
    _      -> fail $ "GrandeurMetier inconnue: " ++ T.unpack t

instance FromJSON GrandeurPhysiqueR63 where
  parseJSON = withText "GrandeurPhysiqueR63" $ \t -> case t of
    "PA"  -> pure GP_PA; "PRI" -> pure GP_PRI
    "PRC" -> pure GP_PRC; "E"  -> pure GP_E
    _     -> fail $ "GrandeurPhysiqueR63 inconnue: " ++ T.unpack t

instance FromJSON EtapeMetier where
  parseJSON = withText "EtapeMetier" $ \t -> case t of
    "BRUT" -> pure BRUT; "BEST" -> pure BEST; "FACT" -> pure FACT
    _      -> fail $ "EtapeMetier inconnue: " ++ T.unpack t

instance FromJSON Pas where
  parseJSON = withText "Pas" $ \t -> case t of
    "PT5M"  -> pure PT5M;  "PT10M" -> pure PT10M; "PT15M" -> pure PT15M
    "PT30M" -> pure PT30M; "PT60M" -> pure PT60M; "P1D"   -> pure P1D
    _       -> fail $ "Pas inconnu: " ++ T.unpack t

instance FromJSON NaturePoint where
  parseJSON = withText "NaturePoint" $ \t -> case t of
    "B" -> pure N_B; "C" -> pure N_C; "R" -> pure N_R; "D" -> pure N_D
    "S" -> pure N_S; "T" -> pure N_T; "F" -> pure N_F; "G" -> pure N_G
    "H" -> pure N_H; "E" -> pure N_E; "P" -> pure N_P
    _   -> fail $ "NaturePoint inconnue: " ++ T.unpack t

instance FromJSON TypeCompletion where
  parseJSON = withText "TypeCompletion" $ \t -> case t of
    "F" -> pure TC_F; "G" -> pure TC_G; "N" -> pure TC_N; "H" -> pure TC_H
    "I" -> pure TC_I; "O" -> pure TC_O; "J" -> pure TC_J; "K" -> pure TC_K
    "L" -> pure TC_L; "C" -> pure TC_C; "A" -> pure TC_A; "M" -> pure TC_M
    _   -> fail $ "TypeCompletion inconnue: " ++ T.unpack t

instance FromJSON ModeCalcul where
  parseJSON = withText "ModeCalcul" $ \t -> case t of
    "MESURE"       -> pure MESURE
    "DIFF.INDEX"   -> pure DIFF_INDEX
    "INTEG.COURBE" -> pure INTEG_COURBE
    _              -> fail $ "ModeCalcul inconnu: " ++ T.unpack t

instance FromJSON ContexteReleve where
  parseJSON = withText "ContexteReleve" $ \t -> case t of
    "COL" -> pure COL; "TOP" -> pure TOP; "FMR" -> pure FMR
    "CRD" -> pure CRD; "CRI" -> pure CRI; "RHF" -> pure RHF
    _     -> fail $ "ContexteReleve inconnu: " ++ T.unpack t

instance FromJSON TypeReleve where
  parseJSON = withText "TypeReleve" $ \t -> case t of
    "AQ" -> pure TR_AQ; "AV" -> pure TR_AV; "AS" -> pure TR_AS; "AP" -> pure TR_AP
    "LC" -> pure TR_LC; "RM" -> pure TR_RM; "RC" -> pure TR_RC
    _    -> fail $ "TypeReleve inconnu: " ++ T.unpack t

-- | Helper : parse un objet contenant "periode" → { "dateDebut", "dateFin" }
parsePeriode :: Object -> Parser Periode
parsePeriode o = do
  p   <- o .: "periode"
  deb <- p .: "dateDebut" >>= parseDateTimeText
  fin <- p .: "dateFin"   >>= parseDateTimeText
  pure $ Periode deb fin

-- ---------------------------------------------------------------------------
-- Conversions vers Text (pour stockage SQLite)

grandeurMetierToText :: GrandeurMetier -> Text
grandeurMetierToText CONS = "CONS"
grandeurMetierToText PROD = "PROD"

grandeurMetierFromText :: Text -> Maybe GrandeurMetier
grandeurMetierFromText "CONS" = Just CONS
grandeurMetierFromText "PROD" = Just PROD
grandeurMetierFromText _      = Nothing

etapeMetierToText :: EtapeMetier -> Text
etapeMetierToText BRUT = "BRUT"
etapeMetierToText BEST = "BEST"
etapeMetierToText FACT = "FACT"

pasToText :: Pas -> Text
pasToText PT5M  = "PT5M"; pasToText PT10M = "PT10M"; pasToText PT15M = "PT15M"
pasToText PT30M = "PT30M"; pasToText PT60M = "PT60M"; pasToText P1D   = "P1D"

pasFromText :: Text -> Maybe Pas
pasFromText "PT5M"  = Just PT5M; pasFromText "PT10M" = Just PT10M
pasFromText "PT15M" = Just PT15M; pasFromText "PT30M" = Just PT30M
pasFromText "PT60M" = Just PT60M; pasFromText "P1D"   = Just P1D
pasFromText _       = Nothing

-- | Durée en secondes d'un pas (pour détection de trous)
pasToSeconds :: Pas -> Int
pasToSeconds PT5M  = 300;  pasToSeconds PT10M = 600;  pasToSeconds PT15M = 900
pasToSeconds PT30M = 1800; pasToSeconds PT60M = 3600; pasToSeconds P1D   = 86400

naturePointToText :: NaturePoint -> Text
naturePointToText N_B = "B"; naturePointToText N_C = "C"; naturePointToText N_R = "R"
naturePointToText N_D = "D"; naturePointToText N_S = "S"; naturePointToText N_T = "T"
naturePointToText N_F = "F"; naturePointToText N_G = "G"; naturePointToText N_H = "H"
naturePointToText N_E = "E"; naturePointToText N_P = "P"

typeCompletionToText :: TypeCompletion -> Text
typeCompletionToText TC_F = "F"; typeCompletionToText TC_G = "G"; typeCompletionToText TC_N = "N"
typeCompletionToText TC_H = "H"; typeCompletionToText TC_I = "I"; typeCompletionToText TC_O = "O"
typeCompletionToText TC_J = "J"; typeCompletionToText TC_K = "K"; typeCompletionToText TC_L = "L"
typeCompletionToText TC_C = "C"; typeCompletionToText TC_A = "A"; typeCompletionToText TC_M = "M"

contexteReleveToText :: ContexteReleve -> Text
contexteReleveToText COL = "COL"; contexteReleveToText TOP = "TOP"; contexteReleveToText FMR = "FMR"
contexteReleveToText CRD = "CRD"; contexteReleveToText CRI = "CRI"; contexteReleveToText RHF = "RHF"

typeReleveToText :: TypeReleve -> Text
typeReleveToText TR_AQ = "AQ"; typeReleveToText TR_AV = "AV"; typeReleveToText TR_AS = "AS"
typeReleveToText TR_AP = "AP"; typeReleveToText TR_LC = "LC"; typeReleveToText TR_RM = "RM"
typeReleveToText TR_RC = "RC"

modeCalculToText :: ModeCalcul -> Text
modeCalculToText MESURE       = "MESURE"
modeCalculToText DIFF_INDEX   = "DIFF_INDEX"
modeCalculToText INTEG_COURBE = "INTEG_COURBE"

grandeurPhysiqueR63ToText :: GrandeurPhysiqueR63 -> Text
grandeurPhysiqueR63ToText GP_PA  = "PA";  grandeurPhysiqueR63ToText GP_PRI = "PRI"
grandeurPhysiqueR63ToText GP_PRC = "PRC"; grandeurPhysiqueR63ToText GP_E   = "E"

grandeurPhysiquePmaxToText :: GrandeurPhysiquePmax -> Text
grandeurPhysiquePmaxToText GP_PMA_MONO = "PMA"; grandeurPhysiquePmaxToText GP_PMA1 = "PMA1"
grandeurPhysiquePmaxToText GP_PMA2     = "PMA2"; grandeurPhysiquePmaxToText GP_PMA3 = "PMA3"

grandeurPhysiqueEnergieToText :: GrandeurPhysiqueEnergie -> Text
grandeurPhysiqueEnergieToText GP_EA_E  = "EA"
grandeurPhysiqueEnergieToText GP_ERI_E = "ERI"
grandeurPhysiqueEnergieToText GP_ERC_E = "ERC"

-- ---------------------------------------------------------------------------
-- Helpers de parsing des dates

parseDateTimeText :: Text -> Parser UTCTime
parseDateTimeText t =
  let s = T.unpack t
      tryFmt fmt = parseTimeM True defaultTimeLocale fmt s
  in case      tryFmt "%Y-%m-%dT%H:%M:%S%z"
           <|> tryFmt "%Y-%m-%dT%H:%M:%S"
           <|> fmap (\d -> UTCTime d 0) (tryFmt "%Y-%m-%d" :: Maybe Day) of
    Just ut -> pure ut
    Nothing -> fail $ "Cannot parse datetime: " ++ s

parseDayText :: Text -> Parser Day
parseDayText t =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just d  -> pure d
    Nothing -> fail $ "Cannot parse day: " ++ T.unpack t
