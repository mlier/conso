module Conso.Fr.Elec.Sge.RechercherPointV20Spec where

import SpecHelper
import TestData
    ( rpCodePostalR1, rpInseeR1, rpDomaineTensionR1
    , rpCodePostalR2, rpInseeR2, rpNomClientR2, rpVoieR2
    , rpCodePostalR3, rpInseeR3, rpVoieR3, rpTypeClienR3 )

import Conso.Fr.Elec.Sge.RechercherPointV20
    ( initTypeTest )
import Conso.Fr.Elec.Sge.RechercherPointV20Type
    ( RechercherPointResponseType )
import           Data.Either (isRight)
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( DomaineTensionCodeType, ClientFinalCategorieCodeType )


shouldRechercherHomo
    :: Maybe String -> Maybe String -> Maybe String -> Maybe String
    -> Maybe String -> Maybe DomaineTensionCodeType -> Maybe String
    -> Maybe ClientFinalCategorieCodeType -> Maybe Bool
    -> Expectation
shouldRechercherHomo voie lieuDit codePostal insee numSiret domaine nom typeClient horsPerimetre =
    pendingOnNetworkError $ do
        myType <- initTypeTest
                    Nothing Nothing voie lieuDit codePostal insee
                    numSiret Nothing domaine nom typeClient horsPerimetre
        rep <- wsRequestTest myType :: IO (Either (String, String) RechercherPointResponseType)
        rep `shouldSatisfy` isRight

shouldRefuserHomo
    :: Maybe String -> Maybe String -> Maybe String -> Maybe String
    -> Maybe String -> Maybe DomaineTensionCodeType -> Maybe String
    -> Maybe ClientFinalCategorieCodeType -> Maybe Bool -> String -> Expectation
shouldRefuserHomo voie lieuDit codePostal insee numSiret domaine nom typeClient horsPerimetre expectedCode =
    pendingOnNetworkError $ do
        myType <- initTypeTest
                    Nothing Nothing voie lieuDit codePostal insee
                    numSiret Nothing domaine nom typeClient horsPerimetre
        rep <- wsRequestTest myType :: IO (Either (String, String) RechercherPointResponseType)
        rep `shouldHaveCode` expectedCode


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "RP-R1 - Recherche par code postal et commune INSEE sans donnée client" $ do
                shouldRechercherHomo
                    Nothing Nothing
                    (Just rpCodePostalR1) (Just rpInseeR1)
                    Nothing (Just rpDomaineTensionR1) Nothing Nothing Nothing

            it "RP-R2 - Recherche par code postal, commune INSEE et nom exact client - fournisseur non titulaire" $ do
                shouldRechercherHomo
                    (Just rpVoieR2) Nothing
                    (Just rpCodePostalR2) (Just rpInseeR2)
                    Nothing Nothing (Just rpNomClientR2) Nothing Nothing

            it "RP-R3 - Recherche d’un point avec adresse exacte et nom approchant" $ do
                shouldRechercherHomo
                    (Just rpVoieR3) Nothing
                    (Just rpCodePostalR3) (Just rpInseeR3)
                    Nothing Nothing (Just "TES") Nothing (Just True)

        describe nonRecevablesC $ do
            it "RP-NR1 - Recherche avec des critères retournant plus de 200 points (SGT4F8)" $ do
                shouldRefuserHomo
                    Nothing Nothing
                    (Just rpCodePostalR3) (Just rpInseeR3)
                    Nothing (Just rpDomaineTensionR1) Nothing (Just rpTypeClienR3)
                    Nothing "SGT4F8"
            it "RP-NR2 - Recherche avec des critères insuffisants (SGT4F7)" $ do
                shouldRefuserHomo
                    (Just rpVoieR3) Nothing
                    Nothing (Just rpInseeR3)
                    Nothing Nothing Nothing Nothing
                    Nothing "SGT4F7"

main :: IO ()
main = hspec spec
