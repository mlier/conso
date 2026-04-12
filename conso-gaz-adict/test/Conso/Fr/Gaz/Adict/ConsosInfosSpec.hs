{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.ConsosInfosSpec where

import SpecHelper
import Data.Either ( isRight, isLeft )

import Conso.Fr.Gaz.Adict.ConsosInfos    ( consulterConsosInfos )
import Conso.Fr.Gaz.Adict.ConsosPubliees ( PeriodeParam(..) )
import TestData


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "ConsosInfo-R01 - Consulter les données de consommation informatives - MM Données informatives (plage)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo1
                                   (ByDateRange dateDebutInfo2 dateFinInfo1)
                    rep `shouldSatisfy` isRight

            it "ConsosInfo-R02 - Consulter les données de consommation informatives - 1M Données informatives (période)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo2
                                   (ByPeriode periodeConsoInfo2)
                    rep `shouldSatisfy` isRight
    describe nonRecevablesC $ do
            it "ConsosInfo-NR03 - Consulter les données de consommation informatives - format du paramètre période incorrect" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo3
                                   (ByPeriode periodeConsoInfo1)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR04 - Consulter les données de consommation informatives - format du paramètre id_pce incorrect" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo4
                                   (ByPeriode periodeConsoInfo2)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR05 - Consulter les données de consommation informatives - erreur technique du serveur GRDF" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo5
                                   (ByPeriode periodeConsoInfo3)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR06 - Consulter les données de consommation informatives - contrat avec GRDF expiré" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo6
                                   (ByPeriode periodeConsoInfo3)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR07 - Consulter les données de consommation informatives - droit d'accès révoqué par le Titulaire" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo7
                                   (ByDateRange dateDebutInfo3 dateFinInfo3)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR08 - Consulter les données de consommation informatives - données informatives hors du périmètre du droit d'accès demandé" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo8
                                   (ByDateRange dateDebutInfo2 dateFinInfo1)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR09 - Consulter les données de consommation informatives - date de début demandée excédant 3 ans d'historique" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo1
                                   (ByDateRange dateDebutInfo1 dateFinInfo1)
                    rep `shouldSatisfy` isLeft  
            it "ConsosInfo-NR10 - Consulter les données de consommation informatives - date de fin supérieure à la date du jour" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterConsosInfos session pceConsoInfo1
                                   (ByDateRange dateDebutInfo2 dateFinInfo2)
                    rep `shouldSatisfy` isLeft  


main :: IO ()
main = hspec spec
