{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Gaps
Description : Détection de trous temporels dans les données SgeDB

Fournit trois fonctions de détection de trous :

  * 'detectCurveGaps' — trous dans les courbes de charge (résultat groupé en 'Periode')
  * 'detectEnergyGaps' — dates manquantes dans les énergies quotidiennes
  * 'detectPmaxGaps'   — dates manquantes dans les Pmax quotidiennes

La détection de trous courbe génère tous les timestamps attendus selon le pas
(via 'pasToSeconds'), les compare aux horodates présentes en base et regroupe
les manquants consécutifs en intervalles 'Periode'.
-}
module Conso.Fr.Elec.SiteDB.Storage.Gaps
  ( detectCurveGaps
  , detectCurveDayGaps
  , detectEnergyGaps
  , detectPmaxGaps
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import           Conso.Fr.Elec.SiteDB.Types.Common (Periode(..), Pas, pasToSeconds)

-- ---------------------------------------------------------------------------
-- Détection de trous dans les courbes de charge

-- | Identifie les intervalles manquants dans les courbes de charge.
-- Génère la séquence complète des horodates attendues d'après le pas,
-- la compare aux horodates présentes en base, puis regroupe les manquants
-- consécutifs en intervalles 'Periode'.
detectCurveGaps
  :: Connection
  -> Text    -- ^ @grandeur_metier@ (@\"CONS\"@ ou @\"PROD\"@)
  -> Text    -- ^ @grandeur_physique@ (@\"PA\"@, @\"PRI\"@, …)
  -> Text    -- ^ @etape_metier@ (@\"BRUT\"@ ou @\"BEST\"@)
  -> Pas     -- ^ Pas attendu des mesures (détermine la fréquence des points)
  -> UTCTime -- ^ Début de la période à vérifier (inclus)
  -> UTCTime -- ^ Fin de la période à vérifier (inclus)
  -> IO [Periode]
detectCurveGaps conn gm gp em pas start end = do
  rows <- query conn
    "SELECT horodate FROM elec_curve_points \
    \ WHERE grandeur_metier = ? AND grandeur_physique = ? AND etape_metier = ? \
    \   AND horodate >= ? AND horodate <= ? \
    \ ORDER BY horodate"
    ( gm, gp, em
    , fmtUTC start, fmtUTC end )
  let present  = Set.fromList $ mapMaybe (parseUTC . fromOnly) rows
      expected = generateTimestamps start end (pasToSeconds pas)
      missing  = filter (`Set.notMember` present) expected
  return $ groupConsecutive (pasToSeconds pas) missing

-- ---------------------------------------------------------------------------
-- Détection de trous dans les courbes de charge (par jour)

-- | Retourne la liste des dates (jours) sans aucun point de courbe dans
-- @elec_curve_points@ pour une grandeur métier donnée.
detectCurveDayGaps
  :: Connection
  -> Text -- ^ @grandeur_metier@ (@\"CONS\"@ ou @\"PROD\"@)
  -> Day  -- ^ Date de début (incluse)
  -> Day  -- ^ Date de fin (incluse)
  -> IO [Day]
detectCurveDayGaps conn gm start end = do
  rows <- query conn
    "SELECT DISTINCT date(horodate) FROM elec_curve_points \
    \ WHERE grandeur_metier = ? AND horodate >= ? AND horodate <= ?"
    (gm, fmtDay start, fmtDay end)
  let present  = Set.fromList $ mapMaybe (parseDay . fromOnly) rows
      expected = [start .. end]
  return $ filter (`Set.notMember` present) expected

-- ---------------------------------------------------------------------------
-- Détection de trous dans les énergies quotidiennes

-- | Retourne la liste des dates manquantes dans @elec_daily_energy@ pour une grandeur.
detectEnergyGaps
  :: Connection
  -> Text -- ^ @grandeur_metier@ (@\"CONS\"@ ou @\"PROD\"@)
  -> Day  -- ^ Date de début (incluse)
  -> Day  -- ^ Date de fin (incluse)
  -> IO [Day]
detectEnergyGaps conn gm start end = do
  rows <- query conn
    "SELECT date_mesure FROM elec_daily_energy \
    \ WHERE grandeur_metier = ? \
    \   AND date_mesure >= ? AND date_mesure <= ? \
    \ ORDER BY date_mesure"
    (gm, showGregorian start, showGregorian end)
  let present  = Set.fromList $ mapMaybe (parseDay . fromOnly) rows
      expected = [start .. end]
  return $ filter (`Set.notMember` present) expected

-- ---------------------------------------------------------------------------
-- Détection de trous dans les Pmax quotidiennes

-- | Retourne la liste des dates manquantes dans @elec_daily_pmax@ pour une grandeur.
-- Groupe par date (une Pmax par jour, quel que soit l'horodate exact).
detectPmaxGaps
  :: Connection
  -> Text -- ^ @grandeur_metier@ (@\"CONS\"@ ou @\"PROD\"@)
  -> Day  -- ^ Date de début (incluse)
  -> Day  -- ^ Date de fin (incluse)
  -> IO [Day]
detectPmaxGaps conn gm start end = do
  rows <- query conn
    "SELECT date(horodate) FROM elec_daily_pmax \
    \ WHERE grandeur_metier = ? \
    \   AND horodate >= ? AND horodate <= ? \
    \ GROUP BY date(horodate) \
    \ ORDER BY date(horodate)"
    (gm, fmtDay start, fmtDay end)
  let present  = Set.fromList $ mapMaybe (parseDay . fromOnly) rows
      expected = [start .. end]
  return $ filter (`Set.notMember` present) expected

-- ---------------------------------------------------------------------------
-- Helpers internes

fmtUTC :: UTCTime -> Text
fmtUTC = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

fmtDay :: Day -> Text
fmtDay = T.pack . showGregorian

parseUTC :: Text -> Maybe UTCTime
parseUTC t = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack t)

parseDay :: Text -> Maybe Day
parseDay t = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)

-- | Génère tous les timestamps attendus entre start et end avec un pas donné (secondes)
generateTimestamps :: UTCTime -> UTCTime -> Int -> [UTCTime]
generateTimestamps start end stepSecs =
  let step = fromIntegral stepSecs :: NominalDiffTime
  in takeWhile (<= end) $ iterate (addUTCTime step) start

-- | Regroupe des timestamps consécutifs manquants en périodes
groupConsecutive :: Int -> [UTCTime] -> [Periode]
groupConsecutive _ [] = []
groupConsecutive stepSecs (t:ts) = go t t ts
  where
    step = fromIntegral stepSecs :: NominalDiffTime
    go gapStart gapEnd [] = [Periode gapStart gapEnd]
    go gapStart gapEnd (x:xs)
      | diffUTCTime x gapEnd <= step * 1.5 = go gapStart x xs
      | otherwise = Periode gapStart gapEnd : go x x xs
