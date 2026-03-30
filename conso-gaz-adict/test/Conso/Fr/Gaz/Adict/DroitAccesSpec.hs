{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DroitAccesSpec where

import SpecHelper
import Data.Either ( isRight )
import Data.Maybe  ( isJust )

import Conso.Fr.Gaz.Adict.DroitAcces
    ( declarerDroitAcces, revoquerDroitAcces )
import Conso.Fr.Gaz.Adict.Types
    ( DemandeAccesIn(..), RetourDemandeAcces(..) )


-- | Demande minimale valide pour le bac à sable GRDF.
demandeMinimale :: DemandeAccesIn
demandeMinimale = DemandeAccesIn
    { din_role_tiers                        = "AUTORISE_CONTRAT_FOURNITURE"
    , din_raison_sociale                    = Nothing
    , din_nom_titulaire                     = Just "Dupont Jean"
    , din_code_postal                       = "75001"
    , din_courriel_titulaire                = Just "test@example.com"
    , din_numero_telephone_mobile_titulaire = Nothing
    , din_date_debut_droit_acces            = Just "2024-01-01"
    , din_date_fin_droit_acces              = Just "2025-12-31"
    , din_perim_donnees_conso_debut         = Just "2024-01-01"
    , din_perim_donnees_conso_fin           = Just "2025-12-31"
    , din_perim_donnees_inj_debut           = Nothing
    , din_perim_donnees_inj_fin             = Nothing
    , din_perim_donnees_contractuelles      = Just True
    , din_perim_donnees_techniques          = Just True
    , din_perim_donnees_informatives        = Just True
    , din_perim_donnees_publiees            = Just True
    }


spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "GDA-R1 - Déclarer un droit d'accès (PUT /pce/{id}/droit_acces)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    pce     <- getTestPce
                    rep     <- declarerDroitAcces session pce demandeMinimale
                    rep `shouldSatisfy` isRight

            it "GDA-R2 - Déclarer puis révoquer un droit d'accès (PUT + PATCH)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    pce     <- getTestPce
                    repDecl <- declarerDroitAcces session pce demandeMinimale
                    repDecl `shouldSatisfy` isRight
                    case repDecl of
                        Left _     -> return ()
                        Right retour -> do
                            rda_id_droit_acces retour `shouldSatisfy` isJust
                            case rda_id_droit_acces retour of
                                Nothing  -> return ()
                                Just uid -> do
                                    repRevoc <- revoquerDroitAcces session uid
                                    repRevoc `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
