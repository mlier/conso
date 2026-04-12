{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.InjectionsPublieesSpec where

import SpecHelper
import Data.Either ( isRight, isLeft )

import Conso.Fr.Gaz.Adict.InjectionsPubliees ( consulterInjectionsPubliees )
import Conso.Fr.Gaz.Adict.ConsosPubliees     ( PeriodeParam(..) )
import TestData


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Inj-R1 - Consulter les données d'injection publiées -  JJ Données publiées" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterInjectionsPubliees session pceInj1
                                   (ByDateRange dateDebutInj2 dateFinInj)
                    rep `shouldSatisfy` isRight
    describe nonRecevablesC $ do
            it "Inj-NR2 - Consulter les données d'injection publiées - droit d'accès ayant un état différent d'Actif, Obsolète ou Révoqué" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterInjectionsPubliees session pceInj2
                                   (ByDateRange dateDebutInj2 dateFinInj)
                    rep `shouldSatisfy` isLeft
            it "Inj-NR3 - Consulter les données d'injection publiées - date de début demandée excédant 5 ans d'historique" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterInjectionsPubliees session pceInj1
                                   (ByDateRange dateDebutInj1 dateFinInj)
                    rep `shouldSatisfy` isLeft
            it "Inj-NR4 - Consulter les données d'injection publiées - date de fin supérieure à la date du jour" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterInjectionsPubliees session pceInj3
                                   (ByDateRange dateDebutInj2 dateFinInj2)
                    rep `shouldSatisfy` isLeft
main :: IO ()
main = hspec spec
