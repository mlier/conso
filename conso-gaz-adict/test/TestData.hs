{-# LANGUAGE OverloadedStrings #-}
-- | Données de référence pour les tests d'intégration ADICT GRDF (bac à sable).
--   Mettre à jour entre deux versions du catalogue GRDF.
module TestData where

import Data.Text (Text)


-- ---------------------------------------------------------------------------
-- PCE de test (bac à sable GRDF)

-- | PCE principal utilisé pour les tests de consommation et données techniques.
sandboxPce :: Text
sandboxPce = "GI461212"

-- | Période de test pour les consommations (année entière récente).
testPeriode :: Text
testPeriode = "2024"

-- | Plage de dates pour les tests avec --debut/--fin.
testDateDebut :: Text
testDateDebut = "2024-01-01"

testDateFin :: Text
testDateFin = "2024-03-31"
