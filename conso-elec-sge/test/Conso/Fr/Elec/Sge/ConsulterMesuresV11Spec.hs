module Conso.Fr.Elec.Sge.ConsulterMesuresV11Spec where

import SpecHelper
import TestData (ahcPrmC5, ahcPrmC1C4)

import Conso.Fr.Elec.Sge.ConsulterMesuresV11
    ( initType, initTypeTest )
import Conso.Fr.Elec.Sge.ConsulterMesuresV11Type
    ( ConsulterMesuresResponseType )
import           Data.Either (isRight, isLeft)


shouldConsulterProd :: String -> Bool -> Expectation
shouldConsulterProd myPointId auth = do
    myType <- initType myPointId auth
    rep <- wsRequest myType :: IO (Either (String, String) ConsulterMesuresResponseType)
    rep `shouldSatisfy` isRight

shouldConsulterHomo :: String -> Bool -> Expectation
shouldConsulterHomo myPointId auth = pendingOnNetworkError $ do
    myType <- initTypeTest myPointId auth
    rep <- wsRequestTest myType :: IO (Either (String, String) ConsulterMesuresResponseType)
    rep `shouldSatisfy` isRight

shouldRefuserHomo :: String -> Bool -> String -> Expectation
shouldRefuserHomo myPointId auth expectedCode = pendingOnNetworkError $ do
    myType <- initTypeTest myPointId auth
    rep <- wsRequestTest myType :: IO (Either (String, String) ConsulterMesuresResponseType)
    rep `shouldSatisfy` isLeft
    case rep of
        Left (code, _) -> code `shouldBe` expectedCode
        Right _        -> expectationFailure "Réponse inattendue : Right"


spec :: Spec
spec = do
    describe productionC $ do
        describe recevablesC $ do
            it "AHC-R1 - Accès à l'historique de consommation (point de production)" $ do
                myPointId <- testPointId
                shouldConsulterProd myPointId True

    describe homologationC $ do
        describe recevablesC $ do
            it "AHC-R1 C5 - Accès à l'historique de consommations avec autorisation client" $ do
                shouldConsulterHomo ahcPrmC5 True

            it "AHC-R1 C1C4 - Accès à l'historique de consommations avec autorisation client" $ do
                shouldConsulterHomo ahcPrmC1C4 True

        describe nonRecevablesC $ do
            it "AHC-NR1 C5 - Accès sans autorisation client (SGT4G2)" $ do
                shouldRefuserHomo ahcPrmC5 False "SGT4G2"


main :: IO ()
main = hspec spec
