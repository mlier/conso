{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Types.Common
Description : Types partagés entre tous les modules SgeDB

Définit les types de base utilisés dans l'ensemble du pipeline :
identifiant PRM, grandeurs métier et physiques, étapes métier, pas de mesure,
natures de point, contextes de relève et la structure 'Periode'.

Ces types sont issus de la nomenclature des flux M023 Enedis
(guide GUI.0503 et GUI.0504).
-}
module Conso.Fr.Site.SiteDB.Elec.Types.Common where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime(..), Day)
import           Data.Time.Format    (parseTimeM, defaultTimeLocale)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Control.Applicative ((<|>))

-- ---------------------------------------------------------------------------
-- Types de base

-- | Identifiant d'un point de mesure (PRM) — 14 chiffres attribués par Enedis.
newtype PrmId = PrmId { unPrmId :: Text }
  deriving (Eq, Ord, Show)

-- | Sens de la mesure.
data GrandeurMetier
  = CONS -- ^ Consommation (Soutirage du réseau)
  | PROD -- ^ Production (Injection vers le réseau)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Grandeur physique pour les courbes de charge R63.
data GrandeurPhysiqueR63
  = GP_PA  -- ^ Puissance Active (W)
  | GP_PRI -- ^ Puissance Réactive Inductive (VAr)
  | GP_PRC -- ^ Puissance Réactive Capacitive (VAr)
  | GP_E   -- ^ Tension (V)
  deriving (Eq, Ord, Show)

-- | Grandeur physique pour les index R64 et mesures facturantes R67.
data GrandeurPhysiqueIndex
  = GP_DD    -- ^ Durée de Dépassement (s)
  | GP_DE    -- ^ Dépassement Énergétique (Wh)
  | GP_DQ    -- ^ Dépassement Quadratique (W)
  | GP_EA    -- ^ Énergie Active (Wh)
  | GP_ER    -- ^ Énergie Réactive (VArh)
  | GP_ERC   -- ^ Énergie Réactive Capacitive (VArh)
  | GP_ERI   -- ^ Énergie Réactive Inductive (VArh)
  | GP_PMA_I -- ^ Puissance Maximale (VA)
  | GP_TF    -- ^ Temps de Fonctionnement (s)
  | GP_PA_IDX -- ^ Puissance Active en index
  deriving (Eq, Ord, Show)

-- | Grandeur physique pour les Pmax R66 (C5\/P4 Linky uniquement, unité VA).
data GrandeurPhysiquePmax
  = GP_PMA_MONO -- ^ Pmax monophasé ou somme des 3 phases (@PMA@)
  | GP_PMA1     -- ^ Pmax phase 1 (@PMA1@, triphasé)
  | GP_PMA2     -- ^ Pmax phase 2 (@PMA2@, triphasé)
  | GP_PMA3     -- ^ Pmax phase 3 (@PMA3@, triphasé)
  deriving (Eq, Ord, Show)

-- | Grandeur physique pour les énergies quotidiennes R65.
data GrandeurPhysiqueEnergie
  = GP_EA_E  -- ^ Énergie Active (Wh)
  | GP_ERI_E -- ^ Énergie Réactive Inductive (VArh)
  | GP_ERC_E -- ^ Énergie Réactive Capacitive (VArh)
  deriving (Eq, Ord, Show)

-- | Étape de traitement d'une mesure.
data EtapeMetier
  = BRUT -- ^ Données brutes issues du compteur, sans correction
  | BEST -- ^ Inclut les points estimés et corrigés (segments C1-C4\/P1-P3 uniquement)
  | FACT -- ^ Données facturantes (R67)
  deriving (Eq, Ord, Show)

-- | Pas de temps d'une mesure (durée ISO 8601).
data Pas
  = PT5M  -- ^ Segments C1-C4 (haute tension)
  | PT10M -- ^ Segments C1-C4 (haute tension)
  | PT15M -- ^ Segments C5\/P4 (Linky résidentiel)
  | PT30M -- ^ Segments C5\/P4 (Linky résidentiel)
  | PT60M -- ^ Segments C5\/P4 (Linky résidentiel)
  | P1D   -- ^ Journalier (R65, R66)
  deriving (Eq, Ord, Show)

-- | Nature d'un point de courbe R63 (champ @n@ du JSON).
data NaturePoint
  = N_B -- ^ Brut Linky
  | N_C -- ^ Corrigé
  | N_R -- ^ Réel (compteur >36 kVA)
  | N_D -- ^ Importé
  | N_S -- ^ Coupure secteur
  | N_T -- ^ Coupure courte
  | N_F -- ^ Début de coupure
  | N_G -- ^ Fin de coupure
  | N_H -- ^ Puissance reconstituée
  | N_E -- ^ Estimé
  | N_P -- ^ Puissance
  deriving (Eq, Ord, Show)

-- | Type de complétion R63 (champ @tc@, présent uniquement si @etapeMetier = BEST@).
data TypeCompletion
  = TC_F -- ^ Corrigé
  | TC_G -- ^ Corrigé
  | TC_N -- ^ Corrigé
  | TC_H -- ^ Corrigé
  | TC_I -- ^ Corrigé
  | TC_O -- ^ Corrigé
  | TC_J -- ^ Corrigé
  | TC_K -- ^ Corrigé
  | TC_L -- ^ Corrigé
  | TC_C -- ^ Estimé
  | TC_A -- ^ Estimé
  | TC_M -- ^ Estimé
  deriving (Eq, Ord, Show)

-- | Contexte de relevé des index R64.
data ContexteReleve
  = COL -- ^ Collecte
  | TOP -- ^ Télé-Opération
  | FMR -- ^ Flux Mesures Régulier (C1-C4)
  | CRD -- ^ Compte-Rendu Demande (C1-C4)
  | CRI -- ^ Intervention
  | RHF -- ^ Relevé Heure Fixe (C1-C4)
  deriving (Eq, Ord, Show)

-- | Type de relevé R64.
data TypeReleve
  = TR_AQ -- ^ Arrêté Quotidien
  | TR_AV -- ^ aVant reprogrammation
  | TR_AS -- ^ aSynchrone
  | TR_AP -- ^ aPrès reprogrammation
  | TR_LC -- ^ Lecture Courante
  | TR_RM -- ^ Relevé Mensuel (C1-C4)
  | TR_RC -- ^ Relevé Courant (C1-C4)
  deriving (Eq, Ord, Show)

-- | Mode de calcul des énergies quotidiennes R65.
data ModeCalcul
  = MESURE       -- ^ Valeur de mesure directe
  | DIFF_INDEX   -- ^ Différence d'index à minuit (C5\/P4)
  | INTEG_COURBE -- ^ Intégrale de la courbe de charge (C1-C4)
  deriving (Eq, Ord, Show)

-- | Mode de publication d'un flux R6X (champ @modePublication@ du header).
data ModePublication
  = MP_Ponctuel     -- ^ @P@ — publication unique (M023)
  | MP_Quotidien    -- ^ @Q@ — service récurrent quotidien (R6X-REC)
  | MP_Hebdomadaire -- ^ @H@ — service récurrent hebdomadaire
  | MP_Mensuel      -- ^ @M@ — service récurrent mensuel
  deriving (Eq, Ord, Show)

-- | Intervalle temporel fonctionnel (bornes incluses).
data Periode = Periode
  { periodeDebut :: UTCTime -- ^ Début de la période (ISO 8601)
  , periodeFin   :: UTCTime -- ^ Fin de la période (ISO 8601)
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

-- | Convertit un 'GrandeurMetier' en sa représentation textuelle SQLite (@\"CONS\"@ ou @\"PROD\"@).
grandeurMetierToText :: GrandeurMetier -> Text
grandeurMetierToText CONS = "CONS"
grandeurMetierToText PROD = "PROD"

-- | Parse un 'GrandeurMetier' depuis son code texte. Retourne 'Nothing' si inconnu.
grandeurMetierFromText :: Text -> Maybe GrandeurMetier
grandeurMetierFromText "CONS" = Just CONS
grandeurMetierFromText "PROD" = Just PROD
grandeurMetierFromText _      = Nothing

-- | Convertit un 'EtapeMetier' en texte (@\"BRUT\"@, @\"BEST\"@, @\"FACT\"@).
etapeMetierToText :: EtapeMetier -> Text
etapeMetierToText BRUT = "BRUT"
etapeMetierToText BEST = "BEST"
etapeMetierToText FACT = "FACT"

-- | Convertit un 'Pas' en son code ISO 8601 (@\"PT5M\"@, @\"PT30M\"@, @\"P1D\"@, …).
pasToText :: Pas -> Text
pasToText PT5M  = "PT5M"; pasToText PT10M = "PT10M"; pasToText PT15M = "PT15M"
pasToText PT30M = "PT30M"; pasToText PT60M = "PT60M"; pasToText P1D   = "P1D"

-- | Parse un 'Pas' depuis son code ISO 8601. Retourne 'Nothing' si inconnu.
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

-- | Parse un 'UTCTime' depuis un texte ISO 8601.
-- Accepte les formats : @YYYY-MM-DDTHH:MM:SS+HH:MM@, @YYYY-MM-DDTHH:MM:SS@, @YYYY-MM-DD@.
parseDateTimeText :: Text -> Parser UTCTime
parseDateTimeText t =
  let s = T.unpack t
      tryFmt fmt = parseTimeM True defaultTimeLocale fmt s
  in case      tryFmt "%Y-%m-%dT%H:%M:%S%z"
           <|> tryFmt "%Y-%m-%dT%H:%M:%S"
           <|> fmap (\d -> UTCTime d 0) (tryFmt "%Y-%m-%d" :: Maybe Day) of
    Just ut -> pure ut
    Nothing -> fail $ "Cannot parse datetime: " ++ s

-- | Parse un 'Day' depuis un texte au format @YYYY-MM-DD@.
parseDayText :: Text -> Parser Day
parseDayText t =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
    Just d  -> pure d
    Nothing -> fail $ "Cannot parse day: " ++ T.unpack t
