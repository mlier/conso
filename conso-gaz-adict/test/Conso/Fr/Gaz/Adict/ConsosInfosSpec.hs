{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.ConsosInfosSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.ConsosInfos    ( consulterConsosInfos )
import Conso.Fr.Gaz.Adict.ConsosPubliees ( PeriodeParam(..) )


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "ConsosInfo-R1 - Consommations informatives par période (année)" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterConsosInfos session pce (ByPeriode "2024")
                rep `shouldSatisfy` isRight

            it "ConsosInfo-R2 - Consommations informatives par plage de dates" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterConsosInfos session pce (ByDateRange "2024-01-01" "2024-03-31")
                rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
