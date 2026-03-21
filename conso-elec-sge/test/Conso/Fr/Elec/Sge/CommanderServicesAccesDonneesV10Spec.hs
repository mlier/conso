module Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Spec where

import SpecHelper
import TestData (sadPrmC5R1, sadPrmC5R2, sadPrmC5R3, sadPrmC5R4, sadPrmC2C4)

import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10
    ( initTypeTest
    , AccordPersonneType(AccordPersonnePhysiqueNom, AccordPersonneMoraleDenominationSociale)
    , Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
    ( CommanderServicesAccesDonneesResponseType )


-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRightOrSgt570 :: Either (String, String) a -> Bool
isRightOrSgt570 (Right _)        = True
isRightOrSgt570 (Left (code, _)) = code == "SGT570"

shouldDemanderC5Homo :: String -> String -> Maybe Integer -> Expectation
shouldDemanderC5Homo prm typeDonnees duree = pendingOnNetworkError $ do
    myType <- initTypeTest prm SensSOUTIRAGE (Just (AccordPersonnePhysiqueNom "Toto")) typeDonnees duree
    rep    <- wsRequestTest myType
                :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    rep `shouldSatisfy` isRightOrSgt570

shouldDemanderC2C4Homo :: String -> String -> Maybe Integer -> Expectation
shouldDemanderC2C4Homo prm typeDonnees duree = pendingOnNetworkError $ do
    myType <- initTypeTest prm SensSOUTIRAGE (Just (AccordPersonneMoraleDenominationSociale "Toto")) typeDonnees duree
    rep    <- wsRequestTest myType
                :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    rep `shouldSatisfy` isRightOrSgt570

shouldRefuserHomo :: String -> String -> String -> Expectation
shouldRefuserHomo prm typeDonnees expectedCode = pendingOnNetworkError $ do
    myType <- initTypeTest prm SensSOUTIRAGE Nothing typeDonnees Nothing
    rep    <- wsRequestTest myType
                :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    rep `shouldHaveCode` expectedCode

shouldRefuserDateFinHomo :: String -> String -> String -> Expectation
shouldRefuserDateFinHomo prm typeDonnees expectedCode = pendingOnNetworkError $ do
    -- dateFin > 3 ans = 1097 jours → SGT509
    myType <- initTypeTest prm SensSOUTIRAGE (Just (AccordPersonnePhysiqueNom "Toto")) typeDonnees (Just 1097)
    rep    <- wsRequestTest myType
                :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    rep `shouldHaveCode` expectedCode


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "SAD-R1 C5    - Accès aux données d'ENERGIES globales quotidiennes" $
                shouldDemanderC5Homo sadPrmC5R1 "ENERGIES" (Just 500)
            it "SAD-R1 C2-C4 - Accès aux données d'ENERGIES globales quotidiennes" $
                shouldDemanderC2C4Homo sadPrmC2C4 "ENERGIES" (Just 500)
            it "SAD-R2 C5    - Accès aux données de CDC (Courbe de charge)" $
                shouldDemanderC5Homo sadPrmC5R2 "COURBE" (Just 500)
            it "SAD-R2 C2-C4 - Accès aux données de CDC (Courbe de charge)" $
                shouldDemanderC2C4Homo sadPrmC2C4 "COURBE" (Just 500)
            it "SAD-R3 C5    - Accès aux données d'INDEX" $
                shouldDemanderC5Homo sadPrmC5R3 "IDX" (Just 500)
            it "SAD-R3 C2-C4 - Accès aux données d'INDEX" $
                shouldDemanderC2C4Homo sadPrmC2C4 "IDX" (Just 500)
            it "SAD-R4 C5    - Accès aux données d'INDEX (multi-service IDX+CDC, IDX seulement)" $
                shouldDemanderC5Homo sadPrmC5R4 "IDX" (Just 500)
            it "SAD-R4 C2-C4 - Accès aux données d'INDEX (multi-service IDX+CDC, IDX seulement)" $
                shouldDemanderC2C4Homo sadPrmC2C4 "IDX" (Just 500)

        describe nonRecevablesC $ do
            it "SAD-NR1 C5 - Sans accord client (SGT566)" $
                shouldRefuserHomo sadPrmC5R1 "ENERGIES" "SGT566"
            it "SAD-NR2 C5 - Date de fin > 3 ans (SGT5O9)" $
                shouldRefuserDateFinHomo sadPrmC5R1 "ENERGIES" "SGT5O9"


main :: IO ()
main = hspec spec
