module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Spec where

import SpecHelper
import TestData (accesPrmC5, accesPrmC2C4)

import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10
    ( initTypeTest
    , AccordPersonneType(AccordPersonnePhysiqueNom)
    , Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( CommanderAccesDonneesMesuresResponseType )
import           Data.Either (isLeft)


-- | Pour les services de commande, SGT570 ("service déjà actif") est
--   également recevable.
isRightOrSgt570 :: Either (String, String) a -> Bool
isRightOrSgt570 (Right _)          = True
isRightOrSgt570 (Left (code, _))   = code == "SGT570"

shouldCommanderHomo :: String -> Maybe Integer -> String -> Expectation
shouldCommanderHomo prm duree typeDonnees = do
    myType <- initTypeTest prm duree (AccordPersonnePhysiqueNom "Toto") typeDonnees SensSOUTIRAGE
    rep    <- wsRequestTest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    rep `shouldSatisfy` isRightOrSgt570

shouldRefuserHomo :: String -> Maybe Integer -> String -> String -> Expectation
shouldRefuserHomo prm duree typeDonnees expectedCode = do
    myType <- initTypeTest prm duree (AccordPersonnePhysiqueNom "Toto") typeDonnees SensSOUTIRAGE
    rep    <- wsRequestTest myType :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
    rep `shouldSatisfy` isLeft
    case rep of
        Left (code, _) -> code `shouldBe` expectedCode
        Right _        -> expectationFailure "Réponse inattendue : Right"


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

            it "ACCES-R4 - Accès ENERGIE C5 sans durée explicite" $ do
                shouldCommanderHomo accesPrmC5 Nothing "ENERGIE"

        describe nonRecevablesC $ do
            it "ACCES-NR2 - Durée supérieure à 3 ans (SGT567)" $ do
                -- La durée dépasse la limite autorisée de 3 ans (1096 jours).
                shouldRefuserHomo accesPrmC5 (Just 1097) "CDC" "SGT567"


main :: IO ()
main = hspec spec
