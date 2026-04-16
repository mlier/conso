{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DroitsAccesSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.DroitsAcces ( consulterDroitsAcces, rechercherDroitsAcces )
import Conso.Fr.Gaz.Adict.Types
    ( FiltreAcces(..)
    , RoleTiers(..)
    , EtatDroitAcces(..)
    , StatutControlePreuve(..)
    )


emptyFiltre :: FiltreAcces
emptyFiltre = FiltreAcces [] [] [] []


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "GDA-R25 - Consulter tous mes droits d'accès" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- consulterDroitsAcces session
                    rep `shouldSatisfy` isRight

            it "GDA-R26 - Consulter mes droits d'accès - avec filtre sur plusieurs PCE" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- rechercherDroitsAcces session
                                   emptyFiltre { fa_id_pce = ["09999999900617", "GI999055"] }
                    rep `shouldSatisfy` isRight

            it "GDA-R27 - Consulter mes droits d'accès - avec filtre sur l'Etat 'Obsolète'" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- rechercherDroitsAcces session
                                   emptyFiltre { fa_etat_droit_acces = [EtatObsolete] }
                    rep `shouldSatisfy` isRight

            it "GDA-R28 - Consulter mes droits d'accès - avec filtre sur le Statut de contrôle 'Preuve en attente' et 'Preuve en cours de vérification'" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- rechercherDroitsAcces session
                                   emptyFiltre { fa_statut_controle_preuve = [PreuveEnAttente, PreuveEnCoursDeVerification] }
                    rep `shouldSatisfy` isRight

            it "GDA-R29 - Consulter mes droits d'accès - avec filtres cumulatifs sur le Role et l'Etat" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- rechercherDroitsAcces session emptyFiltre
                                    { fa_role_tiers       = [DetenteurContratFourniture, DetenteurContratInjection]
                                    , fa_etat_droit_acces = [EtatActive]
                                    }
                    rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
