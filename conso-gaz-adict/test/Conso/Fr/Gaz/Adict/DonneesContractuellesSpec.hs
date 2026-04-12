{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DonneesContractuellesSpec where

import SpecHelper
import Data.Either ( isRight, isLeft )

import Conso.Fr.Gaz.Adict.DonneesContractuelles ( consulterDonneesContractuelles )
import TestData


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Contrat-R1 -  Consulter les données contractuelles" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDonneesContractuelles session pceDonneesContrac []
                    rep `shouldSatisfy` isRight

            it "Contrat-R1bis -  Consulter les données contractuelles avec filtre CAR" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDonneesContractuelles session pceDonneesContrac ["car"]
                    rep `shouldSatisfy` isRight
    describe nonRecevablesC $ do
            it "Contrat-NR2 -  Consulter les données contractuelles - données hors du périmètre du droit d'accès demandé" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDonneesContractuelles session pceDonneesContrac1 []
                    rep `shouldSatisfy` isLeft
            it "Contrat-NR2bis -  Consulter les données contractuelles - données hors du périmètre du droit d'accès demandé avec filtre CAR" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDonneesContractuelles session pceDonneesContrac1 ["car"]
                    rep `shouldSatisfy` isLeft


main :: IO ()
main = hspec spec
