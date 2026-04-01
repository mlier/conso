module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Spec where

import SpecHelper
import TestData (accesPrmC5, accesPrmC2C4)

import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10
    ( initTypeTest
    , AccordPersonneType(AccordPersonnePhysiqueNom)
    , Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( CommanderAccesDonneesMesuresResponseType )

-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRight :: Either (String, String) a -> Bool
isRight (Right _)          = True
isRight (Left _)           = False

shouldCommanderHomo :: String -> Maybe Integer -> String -> Expectation
shouldCommanderHomo prm duree typeDonnees = pendingOnNetworkError $ do
    cleanupServices prm
    myType <- initTypeTest prm duree (Just (AccordPersonnePhysiqueNom "Toto")) typeDonnees SensSOUTIRAGE
    rep    <- wsRequestTest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    rep `shouldSatisfy` isRight

shouldRefuserHomo :: String -> Maybe Integer -> String -> String -> Expectation
shouldRefuserHomo prm duree typeDonnees expectedCode = pendingOnNetworkError $ do
    cleanupServices prm
    myType <- initTypeTest prm duree (Just (AccordPersonnePhysiqueNom "Toto")) typeDonnees SensSOUTIRAGE
    rep    <- wsRequestTest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    rep `shouldHaveCode` expectedCode

shouldRefuserUnlessActiveHomo :: String -> Maybe Integer -> String -> String -> Expectation
shouldRefuserUnlessActiveHomo prm duree typeDonnees expectedCode = pendingOnNetworkError $ do
    cleanupServices prm
    myType <- initTypeTest prm duree (Just (AccordPersonnePhysiqueNom "Toto")) typeDonnees SensSOUTIRAGE
    rep    <- wsRequestTest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    case rep of
        Left ("SGT570", _) -> pendingWith "Service déjà actif (SGT570) : durée non vérifiable sur ce PRM"
        _                  -> rep `shouldHaveCode` expectedCode

shouldRefuserSansAccordHomo :: String -> String -> Expectation
shouldRefuserSansAccordHomo prm expectedCode = pendingOnNetworkError $ do
    cleanupServices prm
    myType <- initTypeTest prm (Just (3 * 365)) Nothing "CDC" SensSOUTIRAGE
    rep    <- wsRequestTest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    rep `shouldHaveCode` expectedCode


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "ACCES-R1 - Accès CDC C5 sur 3 ans" $ do
                shouldCommanderHomo accesPrmC5 (Just (3 * 365)) "CDC"

            it "ACCES-R2 - Accès IDX C5 sur 3 ans" $ do
                shouldCommanderHomo accesPrmC5 (Just (3 * 365)) "IDX"

            it "ACCES-R3 - Accès CDC C2-C4 sur 3 ans" $ do
                shouldCommanderHomo accesPrmC2C4 (Just (3 * 365)) "CDC"

            it "ACCES-R4 - Accès ENERGIE C5 avec durée 3 ans" $ do
                shouldCommanderHomo accesPrmC5 (Just (3 * 365)) "ENERGIE"

        describe nonRecevablesC $ do
            it "ACCES-NR1 - Sans accord client (SGT566)" $ do
                shouldRefuserSansAccordHomo accesPrmC5 "SGT566"

            it "ACCES-NR2 - Durée supérieure à 3 ans (SGT5O9)" $ do
                -- La durée dépasse la limite autorisée de 3 ans (1096 jours).
                shouldRefuserUnlessActiveHomo accesPrmC5 (Just 1500) "CDC" "SGT5O9"


main :: IO ()
main = hspec spec
