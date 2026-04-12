{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DonneesTechniquesSpec where

import SpecHelper
import Data.Either ( isRight, isLeft )

import Conso.Fr.Gaz.Adict.DonneesTechniques ( consulterDonneesTechniques )
import TestData


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Tech-R1 - Consulter les données techniques" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDonneesTechniques session pceDonneesTech
                    rep `shouldSatisfy` isRight
        describe nonRecevablesC $ do
            it "Tech-NR2 - Consulter les données techniques - données hors du périmètre du droit d'accès demandé" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDonneesTechniques session pceDonneesTech1
                    rep `shouldSatisfy` isLeft

main :: IO ()
main = hspec spec
