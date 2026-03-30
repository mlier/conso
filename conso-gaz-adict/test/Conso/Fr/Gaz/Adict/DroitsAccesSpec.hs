{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DroitsAccesSpec where

import SpecHelper
import Data.Either ( isRight )

import Conso.Fr.Gaz.Adict.DroitsAcces ( consulterDroitsAcces, rechercherDroitsAcces )
import Conso.Fr.Gaz.Adict.Types       ( FiltreAcces(..) )


emptyFiltre :: FiltreAcces
emptyFiltre = FiltreAcces
    { fa_role_tiers             = Nothing
    , fa_id_pce                 = Nothing
    , fa_statut_controle_preuve = Nothing
    , fa_etat_droit_acces       = Nothing
    }


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "Droits-R1 - Consulter tous mes droits d'accès (GET)" $ pendingOnAdictError $ do
                session <- sandboxSession
                rep     <- consulterDroitsAcces session
                rep `shouldSatisfy` isRight

            it "Droits-R2 - Rechercher droits d'accès sans filtre (POST)" $ pendingOnAdictError $ do
                session <- sandboxSession
                rep     <- rechercherDroitsAcces session emptyFiltre
                rep `shouldSatisfy` isRight

            it "Droits-R3 - Rechercher droits d'accès par PCE" $ pendingOnAdictError $ do
                session <- sandboxSession
                pce     <- getTestPce
                rep     <- rechercherDroitsAcces session emptyFiltre { fa_id_pce = Just pce }
                rep `shouldSatisfy` isRight

            it "Droits-R4 - Rechercher droits d'accès filtre etat ACTIVE" $ pendingOnAdictError $ do
                session <- sandboxSession
                rep     <- rechercherDroitsAcces session emptyFiltre { fa_etat_droit_acces = Just "ACTIVE" }
                rep `shouldSatisfy` isRight

            it "Droits-R5 - Rechercher droits d'accès filtre statut_controle_preuve" $ pendingOnAdictError $ do
                session <- sandboxSession
                rep     <- rechercherDroitsAcces session emptyFiltre { fa_statut_controle_preuve = Just "VALIDE" }
                rep `shouldSatisfy` isRight

            it "Droits-R6 - Rechercher droits d'accès filtres cumulatifs (role + etat)" $ pendingOnAdictError $ do
                session <- sandboxSession
                rep     <- rechercherDroitsAcces session emptyFiltre
                                { fa_role_tiers       = Just "AUTORISE_CONTRAT_FOURNITURE"
                                , fa_etat_droit_acces = Just "ACTIVE"
                                }
                rep `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
