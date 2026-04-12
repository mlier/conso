{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.ConsosPublieesSpec where

import SpecHelper
import Data.Either ( isRight, isLeft )

import Conso.Fr.Gaz.Adict.ConsosPubliees ( consulterConsosPubliees, PeriodeParam(..) )


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Consos-R01 - Consulter les données de consommation publiées - 1M Données publiées avec données bornées par MES le 02/08/2021" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999900617"
                                   (ByDateRange "2021-01-01" "2021-10-15")
                    rep `shouldSatisfy` isRight

            it "Consos-R02 - Consulter les données de consommation publiées - 6M Données publiées sur pluseurs années avec données bornées par perim_donnees_conso_debut le 15/11/2021" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999975102"
                                   (ByDateRange "2021-01-01" "2023-01-01")
                    rep `shouldSatisfy` isRight

            it "Consos-R03 - Consulter les données de consommation publiées - JJ Données publiées avec changement de fréquence le 10/02/2023" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "GI999055"
                                   (ByDateRange "2023-01-11" "2023-02-11")
                    rep `shouldSatisfy` isRight

            it "Consos-R04 - Consulter les données de consommation publiées - MM Données publiées avec changement de fournisseur le 01/01/2023 et appel periode" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "GI999947"
                                   (ByPeriode "2023-01")
                    rep `shouldSatisfy` isRight

            it "Consos-R05 - Consulter les données de consommation publiées - 1M Données publiées avec changement de compteur le 29/03/2023 et passage à l’état communicant le 14/08/2023" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999928289"
                                   (ByDateRange "2023-03-29" "2023-08-15")
                    rep `shouldSatisfy` isRight

            it "Consos-R06 - Consulter les données de consommation publiées - JJ Données publiées avec changement de tarif le 01/02/2023" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "GI999159"
                                   (ByDateRange "2023-01-31" "2023-02-05")
                    rep `shouldSatisfy` isRight
    
        describe nonRecevablesC $ do
            it "Consos-NR07 - Consulter les données de consommation publiées - erreur sur le Bloc de consommation n°1 (appel date à date)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999975102"
                                   (ByDateRange "2021-08-18" "2023-08-17")
                    rep `shouldSatisfy` isRight

            it "Consos-NR08 - Consulter les données de consommation publiées - PCE sur lequel il n'existe pas de droit d'accès" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999900112"
                                   (ByDateRange "2021-01-01" "2021-10-15")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR09 - Consulter les données de consommation publiées - erreur technique du serveur GRDF" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "GI999159"
                                   (ByPeriode "2023")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR10 - Consulter les données de consommation publiées - MHS du PCE sur lequel il existe un droit d'accès" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "GI999092"
                                   (ByDateRange "2021-08-18" "2023-08-17")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR11 - Consulter les données de consommation publiées - droit d'accès expiré" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999930215"
                                   (ByDateRange "2023-01-01" "2023-09-01")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR12 - Consulter les données de consommation publiées - droit d'accès révoqué par le Titulaire" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999932770"
                                   (ByDateRange "2023-01-01" "2023-02-01")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR13 - Consulter les données de consommation publiées - droit d'accès révoqué par le Tiers" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999970626"
                                   (ByPeriode "2023")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR14 - Consulter les données de consommation publiées - date de début demandée excédant 5 ans d'historique" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999900617"
                                   (ByDateRange "2018-01-01" "2021-10-15")
                    rep `shouldSatisfy` isLeft

            it "Consos-NR15 - Consulter les données de consommation publiées - erreur technique sur le streaming lors de l’exécution de la requête" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosPubliees session "09999999900617"
                                   (ByDateRange "2018-01-01" "2021-10-15")
                    rep `shouldSatisfy` isLeft
main :: IO ()
main = hspec spec
