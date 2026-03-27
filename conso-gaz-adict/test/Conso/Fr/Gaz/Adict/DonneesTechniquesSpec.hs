module Conso.Fr.Gaz.Adict.DonneesTechniquesSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.DonneesTechniques ( consulterDonneesTechniques )


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Tech-R1 - Données techniques d'un PCE" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterDonneesTechniques session pce
                rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
