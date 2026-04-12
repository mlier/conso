{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DroitAccesSpec where

import Control.Monad       ( void, forM_ )
import Data.Either         ( isRight )
import Data.Maybe          ( mapMaybe )

import SpecHelper
import Conso.Fr.Gaz.Adict.DroitAcces
    ( declarerDroitAcces, revoquerDroitAcces )
import Conso.Fr.Gaz.Adict.DroitsAcces ( rechercherDroitsAcces )
import Conso.Fr.Gaz.Adict.Types
    ( DemandeAccesIn(..), FiltreAcces(..)
    , da_id_droit_acces, rda_id_droit_acces )
import TestData


-- | Demande minimale valide pour le bac à sable GRDF (JDD 1 — AUTORISE_CONTRAT_FOURNITURE).
demandeMinimale :: DemandeAccesIn
demandeMinimale = DemandeAccesIn
    { din_role_tiers                        = "AUTORISE_CONTRAT_FOURNITURE"
    , din_raison_sociale                    = Nothing
    , din_nom_titulaire                     = Just "Test 1"
    , din_code_postal                       = "13400"
    , din_courriel_titulaire                = Just "bas_grdf_adict@yopmail.com"
    , din_numero_telephone_mobile_titulaire = Just "0699999999"
    , din_date_debut_droit_acces            = Just "2026-01-01"
    , din_date_fin_droit_acces              = Just "2028-12-31"
    , din_perim_donnees_conso_debut         = Just "2021-01-01"
    , din_perim_donnees_conso_fin           = Just "2028-12-31"
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
            it "GDA-R01 - Déclarer un droit d'accès (PUT /pce/{id}/droit_acces, JDD 1)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    -- Nettoyage préalable : révoquer les droits existants pour ce PCE
                    prior <- rechercherDroitsAcces session
                                 (FiltreAcces [] [pceGdaACF] [] [])
                    forM_ (either (const []) (mapMaybe da_id_droit_acces) prior)
                          (\uuid -> void (revoquerDroitAcces session uuid))
                    -- Déclarer le nouveau droit
                    rep <- declarerDroitAcces session pceGdaACF demandeMinimale
                    rep `shouldSatisfy` isRight
                    -- Nettoyage immédiat pour les prochaines exécutions
                    forM_ (rda_id_droit_acces =<< either (const Nothing) Just rep)
                          (\uuid -> void (revoquerDroitAcces session uuid))

            it "GDA-R2 - Révoquer un droit d'accès (PATCH /droit_acces/{id}, JDD 30)" $
                pendingOnAdictError $ do
                    session  <- sandboxSession
                    repRevoc <- revoquerDroitAcces session uuidRevoquerPassant
                    repRevoc `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
