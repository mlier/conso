{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DonneesContractuellesSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.DonneesContractuelles ( consulterDonneesContractuelles )


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Contrat-R1 - Données contractuelles sans filtre" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterDonneesContractuelles session pce []
                rep `shouldSatisfy` isRight

            it "Contrat-R2 - Données contractuelles avec filtre CAR" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- consulterDonneesContractuelles session pce ["car"]
                rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
