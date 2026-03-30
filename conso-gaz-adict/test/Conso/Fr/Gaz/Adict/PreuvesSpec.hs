{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.Adict.PreuvesSpec where

import SpecHelper
import Data.Either ( isRight )
import qualified Data.ByteString as BS

import Conso.Fr.Gaz.Adict.Preuves    ( soumettrePrevue )
import Conso.Fr.Gaz.Adict.DroitAcces ( declarerDroitAcces )
import Conso.Fr.Gaz.Adict.Types
    ( DemandeAccesIn(..), RetourDemandeAcces(..) )


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
            it "GDA-R3 - Soumettre une preuve (POST /droit_acces/{id}/preuves)" $
                pendingOnAdictError $ do
                    session <- sandboxSession
                    pce     <- getTestPce
                    -- Déclarer un droit d'accès pour obtenir un UUID
                    repDecl <- declarerDroitAcces session pce demandeMinimale
                    repDecl `shouldSatisfy` isRight
                    case repDecl of
                        Left _ -> return ()
                        Right retour ->
                            case rda_id_droit_acces retour of
                                Nothing  -> pendingWith "Pas d'UUID retourné par la déclaration"
                                Just uid -> do
                                    -- Écrire un PDF minimal dans /tmp puis soumettre
                                    let tmpPath = "/tmp/adict-test-preuve.pdf"
                                    BS.writeFile tmpPath minimalPdf
                                    rep <- soumettrePrevue session uid tmpPath
                                    rep `shouldSatisfy` isRight


-- | Contenu PDF minimal valide (header uniquement).
minimalPdf :: BS.ByteString
minimalPdf = "%PDF-1.0\n1 0 obj<</Type /Catalog>>endobj\n%%EOF\n"


main :: IO ()
main = hspec spec
