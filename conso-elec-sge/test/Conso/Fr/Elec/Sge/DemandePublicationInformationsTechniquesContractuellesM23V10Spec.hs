module Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Spec where

import SpecHelper
import TestData (itcPrmsC5, itcPrmsC2C4)

import Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type
    ( AffaireId
    , Sens(Sens_SOUTIRAGE)
    , CadreAcces(CadreAcces_ACCORD_CLIENT) )

-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRightOrSgt570 :: Either (String, String) a -> Bool
isRightOrSgt570 (Right _)          = True
isRightOrSgt570 (Left (code, _))   = code == "SGT570"

shouldDemanderHomo :: [String] -> Expectation
shouldDemanderHomo prms = pendingOnNetworkError $ do
    myType <- initTypeTest prms Sens_SOUTIRAGE CadreAcces_ACCORD_CLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldSatisfy` isRightOrSgt570

shouldRefuserHomo :: [String] -> String -> Expectation
shouldRefuserHomo prms expectedCode = pendingOnNetworkError $ do
    myType <- initTypeTest prms Sens_SOUTIRAGE CadreAcces_ACCORD_CLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldHaveCode` expectedCode


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "ITC-GK-R1 - Demande infos techniques et contractuelles C5" $ do
                shouldDemanderHomo itcPrmsC5

            it "ITC-GK-R2 - Demande infos techniques et contractuelles C2-C4" $ do
                shouldDemanderHomo itcPrmsC2C4

        describe nonRecevablesC $ do
            it "ITC-GK-NR1 - Liste vide de PRMs (SGT4ZM)" $ do
                shouldRefuserHomo [] "SGT4ZM"


main :: IO ()
main = hspec spec
