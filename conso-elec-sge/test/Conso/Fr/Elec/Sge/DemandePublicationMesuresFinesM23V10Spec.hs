module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Spec where

import SpecHelper
import TestData
    ( mfiPrmsC5, mfiPrmsC2C4
    , mfiPrmC5Solo, mfiPrmC2C4Solo
    , mfiDateDebut, mfiDateFin
    , mfiDateDebutNR1, mfiDateDebutNR2 )

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
    ( AffaireId
    , MesuresTypeCode(..)
    , Sens(SensSOUTIRAGE)
    , CadreAcces(CadreAccesACCORDCLIENT) )

-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRightOrSgt570 :: Either (String, String) a -> Bool
isRightOrSgt570 (Right _)          = True
isRightOrSgt570 (Left (code, _))   = code == "SGT570"

shouldDemanderHomo :: [String] -> MesuresTypeCode -> String -> String -> Expectation
shouldDemanderHomo prms typeCode debut fin = pendingOnNetworkError $ do
    myType <- initTypeTest prms typeCode Nothing debut fin SensSOUTIRAGE CadreAccesACCORDCLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldSatisfy` isRightOrSgt570

shouldRefuserHomo :: [String] -> MesuresTypeCode -> String -> String -> String -> Expectation
shouldRefuserHomo prms typeCode debut fin expectedCode = pendingOnNetworkError $ do
    myType <- initTypeTest prms typeCode Nothing debut fin SensSOUTIRAGE CadreAccesACCORDCLIENT
    rep    <- wsRequestTest myType :: IO (Either (String, String) AffaireId)
    rep `shouldHaveCode` expectedCode


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "MFI-GK-R1 C5    - Historique ENERGIE quotidien" $
                shouldDemanderHomo mfiPrmsC5   MesuresTypeCodeENERGIE mfiDateDebut mfiDateFin
            it "MFI-GK-R1 C2-C4 - Historique ENERGIE quotidien" $
                shouldDemanderHomo mfiPrmsC2C4 MesuresTypeCodeENERGIE mfiDateDebut mfiDateFin

            it "MFI-GK-R2 C5    - Historique COURBES (courbe de charge)" $
                shouldDemanderHomo [mfiPrmC5Solo]   MesuresTypeCodeCOURBES mfiDateDebut mfiDateFin
            it "MFI-GK-R2 C2-C4 - Historique COURBES (courbe de charge)" $
                shouldDemanderHomo [mfiPrmC2C4Solo] MesuresTypeCodeCOURBES mfiDateDebut mfiDateFin

            it "MFI-GK-R3 C5    - Historique PMAX (puissances maximales)" $
                shouldDemanderHomo [mfiPrmC5Solo] MesuresTypeCodePMAX mfiDateDebut mfiDateFin

            it "MFI-GK-R4 C5    - Historique INDEX" $
                shouldDemanderHomo mfiPrmsC5   MesuresTypeCodeINDEX mfiDateDebut mfiDateFin
            it "MFI-GK-R4 C2-C4 - Historique INDEX" $
                shouldDemanderHomo mfiPrmsC2C4 MesuresTypeCodeINDEX mfiDateDebut mfiDateFin

        describe nonRecevablesC $ do
            it "MFI-GK-NR1 C5    - Profondeur CDC > 24 mois (SGT4L8)" $
                shouldRefuserHomo mfiPrmsC5   MesuresTypeCodeCOURBES mfiDateDebutNR1 mfiDateFin "SGT4L8"
            it "MFI-GK-NR1 C2-C4 - Profondeur CDC > 24 mois (SGT4L8)" $
                shouldRefuserHomo mfiPrmsC2C4 MesuresTypeCodeCOURBES mfiDateDebutNR1 mfiDateFin "SGT4L8"

            it "MFI-GK-NR2 C5    - Profondeur IDX > 36 mois (SGT4L8)" $
                shouldRefuserHomo mfiPrmsC5   MesuresTypeCodeINDEX mfiDateDebutNR2 mfiDateFin "SGT4L8"
            it "MFI-GK-NR2 C2-C4 - Profondeur IDX > 36 mois (SGT4L8)" $
                shouldRefuserHomo mfiPrmsC2C4 MesuresTypeCodeINDEX mfiDateDebutNR2 mfiDateFin "SGT4L8"


main :: IO ()
main = hspec spec
