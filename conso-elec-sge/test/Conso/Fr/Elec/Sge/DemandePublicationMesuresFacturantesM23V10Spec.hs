module Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10Spec where

import SpecHelper
import TestData
    ( mfaPrmsC5, mfaPrmsC2C4
    , mfaDateDebut, mfaDateFin )

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10Type
    ( AffaireId
    , Sens(Sens_SOUTIRAGE)
    , CadreAcces(CadreAcces_ACCORD_CLIENT) )

-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRightOrSgt570 :: Either (String, String) a -> Bool
isRightOrSgt570 (Right _)          = True
isRightOrSgt570 (Left (code, _))   = code == "SGT570"

shouldDemanderHomo :: [String] -> String -> String -> Expectation
shouldDemanderHomo prms debut fin = pendingOnNetworkError $ do
    myType <- initTypeTest prms debut fin Sens_SOUTIRAGE CadreAcces_ACCORD_CLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldSatisfy` isRightOrSgt570

shouldRefuserHomo :: [String] -> String -> String -> String -> Expectation
shouldRefuserHomo prms debut fin expectedCode = pendingOnNetworkError $ do
    myType <- initTypeTest prms debut fin Sens_SOUTIRAGE CadreAcces_ACCORD_CLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldHaveCode` expectedCode


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "MFA-GK-R1 C5 - Demande mesures facturantes C5" $ do
                shouldDemanderHomo mfaPrmsC5 mfaDateDebut mfaDateFin

            it "MFA-GK-R1 C2-C4 - Demande mesures facturantes C2-C4" $ do
                shouldDemanderHomo mfaPrmsC2C4 mfaDateDebut mfaDateFin

        describe nonRecevablesC $ do
            it "MFA-GK-NR1 C5    - Dates incohérentes : dateFin < dateDebut (SGT4K4)" $ do
                shouldRefuserHomo mfaPrmsC5   mfaDateFin mfaDateDebut "SGT4K4"
            it "MFA-GK-NR1 C2-C4 - Dates incohérentes : dateFin < dateDebut (SGT4K4)" $ do
                shouldRefuserHomo mfaPrmsC2C4 mfaDateFin mfaDateDebut "SGT4K4"


main :: IO ()
main = hspec spec
