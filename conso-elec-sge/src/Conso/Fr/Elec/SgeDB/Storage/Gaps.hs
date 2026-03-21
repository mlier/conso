{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Storage.Gaps
  ( detectCurveGaps
  , detectEnergyGaps
  , detectPmaxGaps
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import           Conso.Fr.Elec.SgeDB.Types.Common (Periode(..), Pas, pasToSeconds)

-- ---------------------------------------------------------------------------
-- Détection de trous dans les courbes de charge

-- | Identifie les intervalles manquants dans les courbes de charge.
-- Génère les horodates attendues d'après le pas, compare avec la base.
detectCurveGaps
  :: Connection
  -> Text    -- grandeur_metier
  -> Text    -- grandeur_physique
  -> Text    -- etape_metier
  -> Pas     -- pas attendu
  -> UTCTime -- début de la période à vérifier
  -> UTCTime -- fin de la période à vérifier
  -> IO [Periode]
detectCurveGaps conn gm gp em pas start end = do
  rows <- query conn
    "SELECT horodate FROM curve_points \
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
-- Détection de trous dans les énergies quotidiennes

detectEnergyGaps
  :: Connection
  -> Text   -- grandeur_metier
  -> Day -> Day
  -> IO [Day]
detectEnergyGaps conn gm start end = do
  rows <- query conn
    "SELECT date_mesure FROM daily_energy \
    \ WHERE grandeur_metier = ? \
    \   AND date_mesure >= ? AND date_mesure <= ? \
    \ ORDER BY date_mesure"
    (gm, showGregorian start, showGregorian end)
  let present  = Set.fromList $ mapMaybe (parseDay . fromOnly) rows
      expected = [start .. end]
  return $ filter (`Set.notMember` present) expected

-- ---------------------------------------------------------------------------
-- Détection de trous dans les Pmax quotidiennes

detectPmaxGaps
  :: Connection
  -> Text   -- grandeur_metier
  -> Day -> Day
  -> IO [Day]
detectPmaxGaps conn gm start end = do
  rows <- query conn
    "SELECT date(horodate) FROM daily_pmax \
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
