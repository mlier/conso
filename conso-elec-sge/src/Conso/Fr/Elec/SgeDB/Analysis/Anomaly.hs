{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Analysis.Anomaly
  ( AnomalyType(..)
  , Anomaly(..)
  , detectAnomalies
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)

-- | Type d'anomalie détectée
data AnomalyType
  = ZScore Double  -- écart à la moyenne en nombre d'écarts-types
  | BorneMin       -- valeur inférieure à la borne minimale
  | BorneMax       -- valeur supérieure à la borne maximale
  deriving (Eq, Show)

-- | Anomalie détectée sur un point de courbe
data Anomaly = Anomaly
  { anomHorodate  :: Text
  , anomValeur    :: Double
  , anomType      :: AnomalyType
  } deriving (Eq, Show)

-- | Détecte les anomalies dans les courbes de charge.
-- Utilise le z-score (seuil configurable) et des bornes absolues optionnelles.
detectAnomalies
  :: Connection
  -> Text           -- grandeur_metier
  -> Text           -- grandeur_physique
  -> Text           -- etape_metier
  -> Text -> Text   -- période (horodate début/fin)
  -> Double         -- seuil z-score (ex: 3.0)
  -> Maybe Double   -- borne minimale absolue
  -> Maybe Double   -- borne maximale absolue
  -> IO [Anomaly]
detectAnomalies conn gm gp em deb fin zThreshold mMin mMax = do
  -- Calcul de la moyenne
  avgRows <- query conn
    "SELECT AVG(CAST(valeur AS REAL)) FROM curve_points \
    \ WHERE grandeur_metier = ? AND grandeur_physique = ? \
    \   AND etape_metier = ? AND horodate >= ? AND horodate <= ?"
    (gm, gp, em, deb, fin)
  let avg = case avgRows of
              [Only (Just a)] -> a :: Double
              _               -> 0

  -- Récupération des points
  rows <- query conn
    "SELECT horodate, CAST(valeur AS REAL) FROM curve_points \
    \ WHERE grandeur_metier = ? AND grandeur_physique = ? \
    \   AND etape_metier = ? AND horodate >= ? AND horodate <= ? \
    \ ORDER BY horodate"
    (gm, gp, em, deb, fin)

  -- Calcul de la variance en Haskell
  let vals    = map snd rows :: [Double]
      n       = length vals
      variance = if n > 0
                   then sum (map (\v -> (v - avg) ^ (2 :: Int)) vals) / fromIntegral n
                   else 1
      stddev  = if variance > 0 then sqrt variance else 1

  return $ concatMap (checkPoint avg stddev) rows
  where
    checkPoint avg stddev (horodate, val) =
      let z    = abs (val - avg) / stddev
          anomZ = [Anomaly horodate val (ZScore z) | z > zThreshold]
          anomMin = case mMin of
            Just mn -> [Anomaly horodate val BorneMin | val < mn]
            Nothing -> []
          anomMax = case mMax of
            Just mx -> [Anomaly horodate val BorneMax | val > mx]
            Nothing -> []
      in anomZ ++ anomMin ++ anomMax
