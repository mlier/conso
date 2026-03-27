{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.ConsosPublieesSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.ConsosPubliees ( consulterConsosPubliees, PeriodeParam(..) )


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Consos-R1 - Consommations publiées par période (année)" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterConsosPubliees session pce (ByPeriode "2024")
                rep `shouldSatisfy` isRight

            it "Consos-R2 - Consommations publiées par mois" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterConsosPubliees session pce (ByPeriode "2024-01")
                rep `shouldSatisfy` isRight

            it "Consos-R3 - Consommations publiées par plage de dates" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterConsosPubliees session pce (ByDateRange "2024-01-01" "2024-03-31")
                rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
