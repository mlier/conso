{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.DroitAccesSpec where

import Control.Monad       ( forM_ )
import Data.Either         ( isRight )
import Data.Maybe          ( mapMaybe )
import Data.Text           ( Text )

import SpecHelper
import Conso.Fr.Gaz.Adict.Adict       ( AdictSession )
import Conso.Fr.Gaz.Adict.DroitAcces
    ( declarerDroitAcces, revoquerDroitAcces )
import Conso.Fr.Gaz.Adict.DroitsAcces ( rechercherDroitsAcces )
import Conso.Fr.Gaz.Adict.Types
    ( DemandeAccesIn(..), FiltreAcces(..), RetourDemandeAcces
    , da_id_droit_acces, rda_id_droit_acces )
import TestData


-- ---------------------------------------------------------------------------
-- Helpers DemandeAccesIn

-- Corps standard AUTORISE_CONTRAT_FOURNITURE avec nom titulaire.
demandeACF :: Text -> Text -> DemandeAccesIn
demandeACF nom cp = DemandeAccesIn
    { din_role_tiers                        = "AUTORISE_CONTRAT_FOURNITURE"
    , din_raison_sociale                    = Nothing
    , din_nom_titulaire                     = Just nom
    , din_code_postal                       = cp
    , din_courriel_titulaire                = Just "bas_grdf_adict@yopmail.com"
    , din_numero_telephone_mobile_titulaire = Just "0699999999"
    , din_date_debut_droit_acces            = Just "2026-01-01"
    , din_date_fin_droit_acces              = Just "2028-12-31"
    , din_perim_donnees_conso_debut         = Just "2021-01-01"
    , din_perim_donnees_conso_fin           = Just "2023-06-30"
    , din_perim_donnees_inj_debut           = Nothing
    , din_perim_donnees_inj_fin             = Nothing
    , din_perim_donnees_contractuelles      = Just "true"
    , din_perim_donnees_techniques          = Just "true"
    , din_perim_donnees_informatives        = Just "true"
    , din_perim_donnees_publiees            = Just "true"
    }

-- Corps AUTORISE_CONTRAT_FOURNITURE avec raison sociale (sans nom_titulaire).
demandeACFRS :: Text -> Text -> DemandeAccesIn
demandeACFRS rs cp = (demandeACF rs cp)
    { din_raison_sociale = Just rs
    , din_nom_titulaire  = Nothing
    }

-- Corps minimal DETENTEUR_CONTRAT_FOURNITURE.
demandeDCF :: Text -> Text -> DemandeAccesIn
demandeDCF rs cp = DemandeAccesIn
    { din_role_tiers                        = "DETENTEUR_CONTRAT_FOURNITURE"
    , din_raison_sociale                    = Just rs
    , din_nom_titulaire                     = Nothing
    , din_code_postal                       = cp
    , din_courriel_titulaire                = Nothing
    , din_numero_telephone_mobile_titulaire = Nothing
    , din_date_debut_droit_acces            = Nothing
    , din_date_fin_droit_acces              = Nothing
    , din_perim_donnees_conso_debut         = Nothing
    , din_perim_donnees_conso_fin           = Nothing
    , din_perim_donnees_inj_debut           = Nothing
    , din_perim_donnees_inj_fin             = Nothing
    , din_perim_donnees_contractuelles      = Nothing
    , din_perim_donnees_techniques          = Nothing
    , din_perim_donnees_informatives        = Nothing
    , din_perim_donnees_publiees            = Nothing
    }

-- Corps AUTORISE_CONTRAT_INJECTION avec périmètre injection.
demandeACI :: Text -> Text -> DemandeAccesIn
demandeACI rs cp = DemandeAccesIn
    { din_role_tiers                        = "AUTORISE_CONTRAT_INJECTION"
    , din_raison_sociale                    = Just rs
    , din_nom_titulaire                     = Nothing
    , din_code_postal                       = cp
    , din_courriel_titulaire                = Just "bas_grdf_adict@yopmail.com"
    , din_numero_telephone_mobile_titulaire = Nothing
    , din_date_debut_droit_acces            = Just "2026-01-01"
    , din_date_fin_droit_acces              = Just "2028-12-31"
    , din_perim_donnees_conso_debut         = Nothing
    , din_perim_donnees_conso_fin           = Nothing
    , din_perim_donnees_inj_debut           = Just "2022-06-10"
    , din_perim_donnees_inj_fin             = Just "2028-12-31"
    , din_perim_donnees_contractuelles      = Just "true"
    , din_perim_donnees_techniques          = Just "true"
    , din_perim_donnees_informatives        = Nothing
    , din_perim_donnees_publiees            = Just "true"
    }

-- Corps minimal DETENTEUR_CONTRAT_INJECTION.
demandeDCI :: Text -> Text -> DemandeAccesIn
demandeDCI rs cp = (demandeDCF rs cp)
    { din_role_tiers = "DETENTEUR_CONTRAT_INJECTION" }


-- ---------------------------------------------------------------------------
-- Helpers nettoyage

preRevoquer :: AdictSession -> Text -> IO ()
preRevoquer session pce = do
    prior <- rechercherDroitsAcces session (FiltreAcces [] [pce] [] [])
    forM_ (either (const []) (mapMaybe da_id_droit_acces) prior) (revoquerDroitAcces session)

postRevoquer :: AdictSession -> Either e RetourDemandeAcces -> IO ()
postRevoquer session rep =
    forM_ (rda_id_droit_acces =<< either (const Nothing) Just rep) (revoquerDroitAcces session)


-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
    describe sandboxC $ do
        describe recevablesC $ do
            it "GDA-R01 - Déclarer ACF 1M (JDD 1, PCE 09999999900617)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaACF
                    rep <- declarerDroitAcces session pceGdaACF
                               (demandeACF "Test 1" "13400")
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R02 - Déclarer ACF 6M (JDD 2, PCE 09999999975102)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaACF2
                    rep <- declarerDroitAcces session pceGdaACF2
                               ((demandeACF "Test 2" "75008")
                                   { din_perim_donnees_contractuelles = Just "false" })
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R03 - Déclarer ACF JJ raison sociale (JDD 3, PCE GI999055)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaACF3
                    rep <- declarerDroitAcces session pceGdaACF3
                               ((demandeACFRS "Test 3" "18200")
                                   { din_perim_donnees_techniques = Just "false" })
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R04 - Déclarer ACF JJ raison sociale (JDD 4, PCE GI999947)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaACF4
                    rep <- declarerDroitAcces session pceGdaACF4
                               ((demandeACFRS "Test 4" "76300")
                                   { din_perim_donnees_informatives = Just "false" })
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R05 - Déclarer DCF (JDD 5, PCE 09999999928289)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaDCF
                    rep <- declarerDroitAcces session pceGdaDCF
                               (demandeDCF "Test 5" "51800")
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R06 - Déclarer ACI données publiées (JDD 6, PCE GI999602)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaACI
                    rep <- declarerDroitAcces session pceGdaACI
                               (demandeACI "Test 6" "72100")
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R07 - Déclarer DCI (JDD 7, PCE GI999150)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    preRevoquer session pceGdaDCI
                    rep <- declarerDroitAcces session pceGdaDCI
                               (demandeDCI "Test 7" "66000")
                    rep `shouldSatisfy` isRight
                    postRevoquer session rep

            it "GDA-R30 - Révoquer un droit d'accès (JDD 30)" $
                pendingOnAdictError $ do
                    session  <- sandboxSession
                    repRevoc <- revoquerDroitAcces session uuidRevoquerPassant
                    repRevoc `shouldSatisfy` isRight

        describe nonRecevablesC $ do
            it "GDA-NR08 - Déclarer : droit d'accès existe déjà (JDD 8)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR08
                                   (demandeACF "Test 8" "24750")
                    rep `shouldBeFunctionalError` "1000000003"

            it "GDA-NR09 - Déclarer un droit d'accès - le rôle n'est pas renseigné" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR09
                                   ((demandeACF "Test 1" "13400")
                                       { din_role_tiers = "" })
                    rep `shouldBeFunctionalError` "2000000006"

            it "GDA-NR10 - Déclarer un droit d'accès - erreur technique du serveur GRDF" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR10
                                   (demandeACF "Test 9" "01020")
                    rep `shouldBeFunctionalError` "2000000001"

            it "GDA-NR11 - Déclarer un droit d'accès - compteur sans contrat actif" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR11
                                   (demandeACF "Test 9" "01020")
                    rep `shouldBeFunctionalError` "1000000004"

            it "GDA-NR12 - Déclarer : code postal incorrect (JDD 12)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR12
                                   (demandeACF "Test 1" "13000")
                    rep `shouldBeFunctionalError` "1000000005"

            it "GDA-NR13 - Déclarer : PCE inconnu (JDD 13)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR13
                                   (demandeACF "Test 1" "13400")
                    rep `shouldBeFunctionalError` "1000000006"

            it "GDA-NR14 - Déclarer : nom et raison sociale non renseignés (JDD 14)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR14
                                   ((demandeACF "Test 1" "13400")
                                       { din_nom_titulaire  = Just ""
                                       , din_raison_sociale = Just "" })
                    rep `shouldBeFunctionalError` "1000000026"

            it "GDA-NR15 - Déclarer : nom et raison sociale tous deux renseignés (JDD 15)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR15
                                   ((demandeACF "Test 1" "13400")
                                       { din_raison_sociale = Just "Test 1" })
                    rep `shouldBeFunctionalError` "1000000039"

            it "GDA-NR16 - Déclarer : code postal incorrect (JDD 16)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR16
                                   (demandeACF "Test 11" "56000")
                    rep `shouldBeFunctionalError` "1000000005"

            it "GDA-NR17 - Déclarer : format PCE incorrect (JDD 17)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR17
                                   (demandeACF "Test 1" "13400")
                    rep `shouldBeFunctionalError` "1000000037"

            it "GDA-NR18 - Déclarer : code postal non renseigné (JDD 18)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR18
                                   (demandeACF "Test 1" "")
                    rep `shouldBeFunctionalError` "1000000027"

            it "GDA-NR19 - Déclarer : format code postal incorrect (JDD 19)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR19
                                   (demandeACF "Test 1" "1340O")
                    rep `shouldBeFunctionalError` "1000000036"

            it "GDA-NR20 - Déclarer : email titulaire non renseigné (JDD 20)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR20
                                   ((demandeACF "Test 1" "13400")
                                       { din_courriel_titulaire = Just "" })
                    rep `shouldBeFunctionalError` "1000000028"

            it "GDA-NR21 - Déclarer : périmètre contractuelles non renseigné (JDD 21)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR21
                                   ((demandeACF "Test 1" "13400")
                                       { din_perim_donnees_contractuelles = Nothing })
                    rep `shouldBeFunctionalError` "1000000031"

            it "GDA-NR22 - Déclarer : date début périmètre conso manquante (JDD 22)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR22
                                   ((demandeACF "Test 1" "13400")
                                       { din_perim_donnees_conso_debut = Just "" })
                    rep `shouldBeFunctionalError` "1000000041"

            it "GDA-NR23 - Déclarer : date fin périmètre injection manquante (JDD 23)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR23
                                   ((demandeACI "Test 6" "72100")
                                       { din_perim_donnees_inj_fin = Just "" })
                    rep `shouldBeFunctionalError` "1000000048"

            it "GDA-NR24 - Déclarer : format email incorrect (JDD 24)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    rep     <- declarerDroitAcces session pceGdaNR24
                                   ((demandeACF "Test 1" "13400")
                                       { din_courriel_titulaire
                                             = Just "bas_grdf_adict@yopmail@com" })
                    rep `shouldBeFunctionalError` "1000000024"

            it "GDA-NR31 - Révoquer : droit inexistant/révoqué (JDD 31)" $
                pendingOnAdictError $ do
                    session  <- sandboxSession
                    repRevoc <- revoquerDroitAcces session uuidRevoquerNR31
                    repRevoc `shouldBeFunctionalError` "1000000001"

            it "GDA-NR32 - Révoquer : PCE utilisé comme UUID (JDD 32)" $
                pendingOnAdictError $ do
                    session  <- sandboxSession
                    repRevoc <- revoquerDroitAcces session uuidRevoquerNR32
                    repRevoc `shouldBeFunctionalError` "2000000001"


main :: IO ()
main = hspec spec
