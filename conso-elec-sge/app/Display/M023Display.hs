{-# OPTIONS_GHC -Wno-orphans #-}

-- | Template d'affichage pour les webservices M023.
module Display.M023Display (AffaireIdResult(..)) where

import           Display


newtype AffaireIdResult = AffaireIdResult String deriving (Show)

instance Renderable AffaireIdResult where
    toWidget (Left (code, msg))            = renderError code msg
    toWidget (Right (AffaireIdResult aid)) =
        section "Demande M023 créée" [field "Identifiant d'affaire" aid]
