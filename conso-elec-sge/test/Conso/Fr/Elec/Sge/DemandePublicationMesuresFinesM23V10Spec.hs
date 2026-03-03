module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Spec where

import SpecHelper
import TestData
    ( mfiPrmsC5, mfiPrmsC2C4
    , mfiDateDebut, mfiDateFin )

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
    ( AffaireId
    , MesuresTypeCode(..)
    , Sens(SensSOUTIRAGE)
    , CadreAcces(CadreAccesACCORDCLIENT) )
import           Data.Either (isLeft)


-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRightOrSgt570 :: Either (String, String) a -> Bool
isRightOrSgt570 (Right _)          = True
isRightOrSgt570 (Left (code, _))   = code == "SGT570"

shouldDemanderHomo :: [String] -> MesuresTypeCode -> String -> String -> Expectation
shouldDemanderHomo prms typeCode debut fin = do
    myType <- initTypeTest prms typeCode Nothing debut fin SensSOUTIRAGE CadreAccesACCORDCLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldSatisfy` isRightOrSgt570

shouldRefuserHomo :: [String] -> MesuresTypeCode -> String -> String -> String -> Expectation
shouldRefuserHomo prms typeCode debut fin expectedCode = do
    myType <- initTypeTest prms typeCode Nothing debut fin SensSOUTIRAGE CadreAccesACCORDCLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldSatisfy` isLeft
    case rep of
        Left (code, _) -> code `shouldBe` expectedCode
        Right _        -> expectationFailure "Réponse inattendue : Right"


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "MFI-GK-R1 - Demande INDEX C5" $ do
                shouldDemanderHomo mfiPrmsC5 MesuresTypeCodeINDEX mfiDateDebut mfiDateFin

            it "MFI-GK-R2 - Demande INDEX C2-C4" $ do
                shouldDemanderHomo mfiPrmsC2C4 MesuresTypeCodeINDEX mfiDateDebut mfiDateFin

            it "MFI-GK-R3 - Demande COURBES C5" $ do
                shouldDemanderHomo mfiPrmsC5 MesuresTypeCodeCOURBES mfiDateDebut mfiDateFin

            it "MFI-GK-R4 - Demande ENERGIE C5" $ do
                shouldDemanderHomo mfiPrmsC5 MesuresTypeCodeENERGIE mfiDateDebut mfiDateFin

        describe nonRecevablesC $ do
            it "MFI-GK-NR1 - Liste vide de PRMs (SGT4ZM)" $ do
                shouldRefuserHomo [] MesuresTypeCodeINDEX mfiDateDebut mfiDateFin "SGT4ZM"


main :: IO ()
main = hspec spec
