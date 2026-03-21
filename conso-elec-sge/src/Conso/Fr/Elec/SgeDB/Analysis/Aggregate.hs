{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Analysis.Aggregate
  ( AggregationPeriod(..)
  , AggregateRow(..)
  , aggregateCurve
  , aggregateEnergy
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)

-- | Granularité d'agrégation temporelle
data AggregationPeriod = ParJour | ParSemaine | ParMois | ParAn
  deriving (Eq, Show)

-- | Résultat d'agrégation
data AggregateRow = AggregateRow
  { agPeriode :: Text    -- label de la période (YYYY-MM-DD, YYYY-Www, YYYY-MM, YYYY)
  , agSomme   :: Double
  , agMoyenne :: Double
  , agMax     :: Double
  , agNbPoints :: Int
  } deriving (Eq, Show)

instance FromRow AggregateRow where
  fromRow = AggregateRow <$> field <*> field <*> field <*> field <*> field

-- | Agrège les courbes de charge sur une période
aggregateCurve
  :: Connection
  -> Text            -- grandeur_metier
  -> Text            -- grandeur_physique
  -> Text            -- etape_metier
  -> AggregationPeriod
  -> Text -> Text    -- horodate début/fin
  -> IO [AggregateRow]
aggregateCurve conn gm gp em period deb fin =
  query conn
    (Query $
      "SELECT " <> periodExpr period <> " AS periode, \
      \ SUM(CAST(valeur AS REAL)), \
      \ AVG(CAST(valeur AS REAL)), \
      \ MAX(CAST(valeur AS REAL)), \
      \ COUNT(*) \
      \ FROM curve_points \
      \ WHERE grandeur_metier = ? AND grandeur_physique = ? \
      \   AND etape_metier = ? \
      \   AND horodate >= ? AND horodate <= ? \
      \ GROUP BY periode ORDER BY periode")
    (gm, gp, em, deb, fin)

-- | Agrège les énergies quotidiennes sur une période
aggregateEnergy
  :: Connection
  -> Text
  -> Text
  -> AggregationPeriod
  -> Text -> Text
  -> IO [AggregateRow]
aggregateEnergy conn gm gp period deb fin =
  query conn
    (Query $
      "SELECT " <> periodExprDay period <> " AS periode, \
      \ SUM(CAST(valeur AS REAL)), \
      \ AVG(CAST(valeur AS REAL)), \
      \ MAX(CAST(valeur AS REAL)), \
      \ COUNT(*) \
      \ FROM daily_energy \
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
