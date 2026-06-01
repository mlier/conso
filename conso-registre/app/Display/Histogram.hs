{-# LANGUAGE OverloadedStrings #-}
module Display.Histogram
  ( barChart
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Granite (bars, defPlot, Plot(..), LegendPos(..), LabelFormatter)
import Granite.Format (Formatter(..), runFormatter)

-- | Affiche un bar chart en terminal via la bibliothèque granite.
barChart :: Text -> [(Text, Double)] -> IO ()
barChart _     []    = putStrLn "(aucune donnée)"
barChart title pairs =
  TIO.putStrLn $ bars (thinLabels pairs) defPlot
    { plotTitle  = title
    , widthChars = 100
    , legendPos  = LegendNone
    , yFormatter = intFormatter
    }

-- | Conserve ~10 labels espacés régulièrement, remplace les autres par "".
-- Granite affiche la barre mais sans texte pour les labels vides.
thinLabels :: [(Text, Double)] -> [(Text, Double)]
thinLabels pairs
  | n <= 30   = pairs
  | otherwise = [ (if i `mod` step == 0 then lbl else "", val)
                | (i, (lbl, val)) <- zip [0..] pairs ]
  where
    n    = length pairs
    step = max 1 (n `div` 10)

intFormatter :: LabelFormatter
intFormatter _ _ val = runFormatter FormatComma val
