{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Analysis.Aggregate
Description : Agrégation temporelle des mesures SgeDB (par jour, semaine, mois, an)

Fournit 'aggregateCurve' et 'aggregateEnergy' pour calculer somme, moyenne,
maximum et nombre de points par période d'agrégation.

Le label de la période dans 'AggregateRow' a le format :

  * 'ParJour'    — @YYYY-MM-DD@
  * 'ParSemaine' — @YYYY-Www@ (semaine ISO)
  * 'ParMois'    — @YYYY-MM@
  * 'ParAn'      — @YYYY@
-}
module Conso.Fr.Elec.SiteDB.Analysis.Aggregate
  ( AggregationPeriod(..)
  , AggregateRow(..)
  , aggregateCurve
  , aggregateEnergy
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)

-- | Granularité d'agrégation temporelle.
data AggregationPeriod
  = ParJour    -- ^ Agrégation par jour — label @YYYY-MM-DD@
  | ParSemaine -- ^ Agrégation par semaine ISO — label @YYYY-Www@
  | ParMois    -- ^ Agrégation par mois — label @YYYY-MM@
  | ParAn      -- ^ Agrégation par année — label @YYYY@
  deriving (Eq, Show)

-- | Résultat d'agrégation pour une période.
data AggregateRow = AggregateRow
  { agPeriode  :: Text   -- ^ Label de la période (format dépend de 'AggregationPeriod')
  , agSomme    :: Double -- ^ Somme des valeurs sur la période
  , agMoyenne  :: Double -- ^ Moyenne des valeurs
  , agMax      :: Double -- ^ Maximum des valeurs
  , agNbPoints :: Int    -- ^ Nombre de points inclus dans l'agrégat
  } deriving (Eq, Show)

instance FromRow AggregateRow where
  fromRow = AggregateRow <$> field <*> field <*> field <*> field <*> field

-- | Agrège les courbes de charge (@elec_curve_points@) sur une période.
aggregateCurve
  :: Connection
  -> Text              -- ^ @grandeur_metier@ (@\"CONS\"@ ou @\"PROD\"@)
  -> Text              -- ^ @grandeur_physique@ (@\"PA\"@, @\"PRI\"@, …)
  -> Text              -- ^ @etape_metier@ (@\"BRUT\"@ ou @\"BEST\"@)
  -> AggregationPeriod -- ^ Granularité d'agrégation
  -> Text              -- ^ Horodate début (ISO 8601)
  -> Text              -- ^ Horodate fin (ISO 8601)
  -> IO [AggregateRow]
aggregateCurve conn gm gp em period deb fin =
  query conn
    (Query $
      "SELECT " <> periodExpr period <> " AS periode, \
      \ SUM(CAST(valeur AS REAL)), \
      \ AVG(CAST(valeur AS REAL)), \
      \ MAX(CAST(valeur AS REAL)), \
      \ COUNT(*) \
      \ FROM elec_curve_points \
      \ WHERE grandeur_metier = ? AND grandeur_physique = ? \
      \   AND etape_metier = ? \
      \   AND horodate >= ? AND horodate <= ? \
      \ GROUP BY periode ORDER BY periode")
    (gm, gp, em, deb, fin)

-- | Agrège les énergies quotidiennes (@elec_daily_energy@) sur une période.
aggregateEnergy
  :: Connection
  -> Text              -- ^ @grandeur_metier@
  -> Text              -- ^ @grandeur_physique@ (@\"EA\"@, @\"ERI\"@, @\"ERC\"@)
  -> AggregationPeriod -- ^ Granularité d'agrégation
  -> Text              -- ^ Date début (@YYYY-MM-DD@)
  -> Text              -- ^ Date fin (@YYYY-MM-DD@)
  -> IO [AggregateRow]
aggregateEnergy conn gm gp period deb fin =
  query conn
    (Query $
      "SELECT " <> periodExprDay period <> " AS periode, \
      \ SUM(CAST(valeur AS REAL)), \
      \ AVG(CAST(valeur AS REAL)), \
      \ MAX(CAST(valeur AS REAL)), \
      \ COUNT(*) \
      \ FROM elec_daily_energy \
      \ WHERE grandeur_metier = ? AND grandeur_physique = ? \
      \   AND date_mesure >= ? AND date_mesure <= ? \
      \ GROUP BY periode ORDER BY periode")
    (gm, gp, deb, fin)

-- | Expression SQLite pour la période d'agrégation sur un timestamp
periodExpr :: AggregationPeriod -> Text
periodExpr ParJour    = "date(horodate)"
periodExpr ParSemaine = "strftime('%Y-W%W', horodate)"
periodExpr ParMois    = "strftime('%Y-%m', horodate)"
periodExpr ParAn      = "strftime('%Y', horodate)"

-- | Expression SQLite pour la période d'agrégation sur une date
periodExprDay :: AggregationPeriod -> Text
periodExprDay ParJour    = "date_mesure"
periodExprDay ParSemaine = "strftime('%Y-W%W', date_mesure)"
periodExprDay ParMois    = "strftime('%Y-%m', date_mesure)"
periodExprDay ParAn      = "strftime('%Y', date_mesure)"
