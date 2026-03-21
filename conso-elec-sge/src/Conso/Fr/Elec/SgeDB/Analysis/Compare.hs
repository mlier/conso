{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Analysis.Compare
Description : Comparaison de deux périodes sur une même grandeur

Fournit 'comparePeriods' pour calculer les statistiques (somme, moyenne, max)
sur deux périodes distinctes et la variation relative entre elles :

> variation = (somme_p2 - somme_p1) / somme_p1 * 100  -- en %
-}
module Conso.Fr.Elec.SgeDB.Analysis.Compare
  ( ComparisonResult(..)
  , comparePeriods
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)

-- | Résultat de comparaison de deux périodes sur une même grandeur.
data ComparisonResult = ComparisonResult
  { cmpPeriode1Somme :: Double -- ^ Somme des valeurs sur la période de référence
  , cmpPeriode2Somme :: Double -- ^ Somme des valeurs sur la période de comparaison
  , cmpVariation     :: Double -- ^ Variation relative : @(p2 - p1) \/ p1 * 100@ (en %) ; @0@ si p1 = 0
  , cmpPeriode1Moy   :: Double -- ^ Moyenne sur la période de référence
  , cmpPeriode2Moy   :: Double -- ^ Moyenne sur la période de comparaison
  , cmpPeriode1Max   :: Double -- ^ Maximum sur la période de référence
  , cmpPeriode2Max   :: Double -- ^ Maximum sur la période de comparaison
  } deriving (Eq, Show)

-- | Compare deux périodes sur une même grandeur de courbe de charge.
comparePeriods
  :: Connection
  -> Text         -- ^ @grandeur_metier@
  -> Text         -- ^ @grandeur_physique@
  -> Text         -- ^ @etape_metier@
  -> (Text, Text) -- ^ Période de référence (début, fin) — ISO 8601
  -> (Text, Text) -- ^ Période de comparaison (début, fin) — ISO 8601
  -> IO ComparisonResult
comparePeriods conn gm gp em (deb1, fin1) (deb2, fin2) = do
  (s1, avg1, mx1) <- queryStats conn gm gp em deb1 fin1
  (s2, avg2, mx2) <- queryStats conn gm gp em deb2 fin2
  let variation = if s1 == 0 then 0 else (s2 - s1) / s1 * 100
  return $ ComparisonResult s1 s2 variation avg1 avg2 mx1 mx2

queryStats :: Connection -> Text -> Text -> Text -> Text -> Text
           -> IO (Double, Double, Double)
queryStats conn gm gp em deb fin = do
  rows <- query conn
    "SELECT COALESCE(SUM(CAST(valeur AS REAL)), 0), \
    \       COALESCE(AVG(CAST(valeur AS REAL)), 0), \
    \       COALESCE(MAX(CAST(valeur AS REAL)), 0) \
    \ FROM curve_points \
    \ WHERE grandeur_metier = ? AND grandeur_physique = ? \
    \   AND etape_metier = ? \
    \   AND horodate >= ? AND horodate <= ?"
    (gm, gp, em, deb, fin)
  return $ case rows of
    [(s, a, m)] -> (s, a, m)
    _           -> (0, 0, 0)
