module Conso.Fr.Elec.Sge.RechercherPointV20Spec where

import SpecHelper
import TestData
    ( rpCodePostalR1, rpInseeR1
    , rpCodePostalR2, rpInseeR2, rpNomClientR2
    , rpCodePostalR3, rpInseeR3, rpVoieR3 )

import Conso.Fr.Elec.Sge.RechercherPointV20
    ( initTypeTest )
import Conso.Fr.Elec.Sge.RechercherPointV20Type
    ( RechercherPointResponseType )
import           Data.Either (isRight, isLeft)


-- | Recherche par code postal + code INSEE de commune.
shouldRechercherHomo
    :: Maybe String -> Maybe String -> Maybe String -> Maybe String
    -> Maybe String -> Maybe String -> Maybe Bool
    -> Expectation
shouldRechercherHomo voie lieuDit codePostal insee numSiret nom horsPerimetre = do
    myType <- initTypeTest
                Nothing Nothing voie lieuDit codePostal insee
                numSiret Nothing Nothing nom Nothing horsPerimetre
    rep <- wsRequestTest myType :: IO (Either (String, String) RechercherPointResponseType)
    rep `shouldSatisfy` isRight

shouldRefuserHomo
    :: Maybe String -> Maybe String -> Maybe String -> Maybe String
    -> Maybe String -> Maybe String -> Maybe Bool
    -> String -> Expectation
shouldRefuserHomo voie lieuDit codePostal insee numSiret nom horsPerimetre expectedCode = do
    myType <- initTypeTest
                Nothing Nothing voie lieuDit codePostal insee
                numSiret Nothing Nothing nom Nothing horsPerimetre
    rep <- wsRequestTest myType :: IO (Either (String, String) RechercherPointResponseType)
    rep `shouldSatisfy` isLeft
    case rep of
        Left (code, _) -> code `shouldBe` expectedCode
        Right _        -> expectationFailure "Réponse inattendue : Right"


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "RP-R1 - Recherche par code postal et commune INSEE" $ do
                shouldRechercherHomo
                    Nothing Nothing
                    (Just rpCodePostalR1) (Just rpInseeR1)
                    Nothing Nothing Nothing

            it "RP-R2 - Recherche par code postal, commune INSEE et nom client" $ do
                shouldRechercherHomo
                    Nothing Nothing
                    (Just rpCodePostalR2) (Just rpInseeR2)
                    Nothing (Just rpNomClientR2) Nothing

            it "RP-R3 - Recherche par code postal, commune INSEE et voie" $ do
                shouldRechercherHomo
                    (Just rpVoieR3) Nothing
                    (Just rpCodePostalR3) (Just rpInseeR3)
                    Nothing Nothing Nothing

        describe nonRecevablesC $ do
            it "RP-NR1 - Critères insuffisants : code postal seul (SGT4ZH)" $ do
                shouldRefuserHomo
                    Nothing Nothing
                    (Just rpCodePostalR1) Nothing
                    Nothing Nothing Nothing
                    "SGT4ZH"


main :: IO ()
main = hspec spec
