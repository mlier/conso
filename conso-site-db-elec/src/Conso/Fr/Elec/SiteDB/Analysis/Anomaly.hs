{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Analysis.Anomaly
Description : Détection d'anomalies dans les courbes de charge

Fournit 'detectAnomalies' qui combine deux approches de détection :

  1. __Z-score__ — signale les points dont l'écart à la moyenne dépasse
     un seuil (ex. 3 écarts-types)
  2. __Bornes absolues__ — signale les points en dehors d'une plage [min, max]

L'algorithme calcule d'abord la moyenne puis la variance en mémoire Haskell
pour éviter deux requêtes SQL (la variance n'est pas native en SQLite standard).
-}
module Conso.Fr.Elec.SiteDB.Analysis.Anomaly
  ( AnomalyType(..)
  , Anomaly(..)
  , detectAnomalies
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)

-- | Type d'anomalie détectée sur un point de courbe.
data AnomalyType
  = ZScore Double -- ^ Anomalie statistique : valeur portant le z-score (nombre d'écarts-types)
  | BorneMin      -- ^ Valeur inférieure à la borne minimale absolue fournie
  | BorneMax      -- ^ Valeur supérieure à la borne maximale absolue fournie
  deriving (Eq, Show)

-- | Anomalie détectée sur un point de courbe de charge.
data Anomaly = Anomaly
  { anomHorodate :: Text        -- ^ Horodate du point anormal (ISO 8601)
  , anomValeur   :: Double      -- ^ Valeur du point anormal
  , anomType     :: AnomalyType -- ^ Type et détail de l'anomalie
  } deriving (Eq, Show)

-- | Détecte les anomalies dans les courbes de charge.
--
-- Algorithme : calcul de la moyenne → variance → écart-type → z-score par point.
-- Un point est anormal si son z-score dépasse @zThreshold@ ou s'il est hors bornes.
detectAnomalies
  :: Connection
  -> Text         -- ^ @grandeur_metier@
  -> Text         -- ^ @grandeur_physique@
  -> Text         -- ^ @etape_metier@
  -> Text         -- ^ Horodate début (ISO 8601)
  -> Text         -- ^ Horodate fin (ISO 8601)
  -> Double       -- ^ Seuil z-score (valeur typique : @3.0@)
  -> Maybe Double -- ^ Borne minimale absolue (optionnelle)
  -> Maybe Double -- ^ Borne maximale absolue (optionnelle)
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
