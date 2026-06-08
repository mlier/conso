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
    , widthChars = adjustWidth (length pairs)
    , legendPos  = LegendNone
    , yFormatter = intFormatter
    }

-- | Arrondit widthChars au prochain multiple de n pour que keepPercentiles
-- de Granite produise une bijection position→slot (k = n), évitant les
-- collisions qui font disparaître certains labels (ex : semaine 05 sur 19).
-- Pour n ≤ 17 : widthChars=100 suffit nativement (k=n déjà satisfait).
-- Pour n > 30 : thinLabels réduit à ~10 labels effectifs, pas de collision.
adjustWidth :: Int -> Int
adjustWidth n
  | n <= 17   = 100
  | n <= 30   = n * (100 `div` n + 1)
  | otherwise = 100

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
