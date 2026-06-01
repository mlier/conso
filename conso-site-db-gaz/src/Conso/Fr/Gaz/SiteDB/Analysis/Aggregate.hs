{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.SiteDB.Analysis.Aggregate
  ( AggregationPeriod(..)
  , GazAggRow(..)
  , aggregateGazConso
  , aggregateGazConsoInfo
  , aggregateGazInjection
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)

-- | Granularité d'agrégation temporelle.
data AggregationPeriod
  = ParJour  -- ^ Agrégation par jour — label @YYYY-MM-DD@
  | ParMois  -- ^ Agrégation par mois — label @YYYY-MM@
  | ParAn    -- ^ Agrégation par année — label @YYYY@
  deriving (Eq, Show)

-- | Résultat d'agrégation gaz pour une période.
data GazAggRow = GazAggRow
  { gazAgPeriode  :: Text   -- ^ Label de la période
  , gazAgSomme    :: Double -- ^ Somme énergie (kWh) sur la période
  , gazAgNbPoints :: Int    -- ^ Nombre de points inclus
  } deriving (Eq, Show)

instance FromRow GazAggRow where
  fromRow = GazAggRow <$> field <*> field <*> field

-- | Agrège les consommations publiées (@gaz_conso@) sur une période.
aggregateGazConso
  :: Connection
  -> AggregationPeriod
  -> Text              -- ^ Date début (@YYYY-MM-DD@)
  -> Text              -- ^ Date fin (@YYYY-MM-DD@)
  -> IO [GazAggRow]
aggregateGazConso conn = aggregateTable conn "gaz_conso"

-- | Agrège les consommations informatives (@gaz_conso_informative@) sur une période.
aggregateGazConsoInfo
  :: Connection
  -> AggregationPeriod
  -> Text              -- ^ Date début (@YYYY-MM-DD@)
  -> Text              -- ^ Date fin (@YYYY-MM-DD@)
  -> IO [GazAggRow]
aggregateGazConsoInfo conn = aggregateTable conn "gaz_conso_informative"

-- | Agrège les injections publiées (@gaz_injection@) sur une période.
aggregateGazInjection
  :: Connection
  -> AggregationPeriod
  -> Text              -- ^ Date début (@YYYY-MM-DD@)
  -> Text              -- ^ Date fin (@YYYY-MM-DD@)
  -> IO [GazAggRow]
aggregateGazInjection conn = aggregateTable conn "gaz_injection"

aggregateTable :: Connection -> Text -> AggregationPeriod -> Text -> Text -> IO [GazAggRow]
aggregateTable conn table period deb fin =
  query conn
    (Query $
      "SELECT " <> periodExpr period <> " AS periode, \
      \ SUM(energie_kwh), \
      \ COUNT(*) \
      \ FROM " <> table <>
      " WHERE debut >= ? AND debut <= ? \
      \ GROUP BY periode ORDER BY periode")
    (deb, fin)

periodExpr :: AggregationPeriod -> Text
periodExpr ParJour = "date(debut)"
periodExpr ParMois = "strftime('%Y-%m', debut)"
periodExpr ParAn   = "strftime('%Y', debut)"
