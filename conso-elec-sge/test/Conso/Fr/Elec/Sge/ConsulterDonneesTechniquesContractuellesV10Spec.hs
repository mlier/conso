module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Spec where

import SpecHelper
    ( hspec,
      describe,
      it,
      shouldSatisfy,
      Spec,
      Expectation,
      wsRequest,
      wsRequestTest,
      testPointId,
      productionC,
      homologationC,
      recevablesC,
      nonRecevablesC,
      pendingOnNetworkError,
      shouldHaveCode )
import TestData (adpPrmC5, adpPrmC1C4)


import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10
    ( initType, initTypeTest )
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
    ( ConsulterDonneesTechniquesContractuellesResponseType )
import           Data.Either (isRight)


shouldConsulterProd :: String -> Bool -> Expectation
shouldConsulterProd myPointId auth = do
    myType <- initType myPointId auth
    rep <- wsRequest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
    rep `shouldSatisfy` isRight

shouldConsulterHomo :: String -> Bool -> Expectation
shouldConsulterHomo myPointId auth = pendingOnNetworkError $ do
    myType <- initTypeTest myPointId auth
    rep <- wsRequestTest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    let pointIdUnkown = "99999999999999"

    describe productionC $ do
        describe recevablesC $ do
            it "ADP-R1 C5 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
                myPointId <- testPointId
                shouldConsulterProd myPointId True

            it "ADP-R2 C5 - Accès aux données d’un point en service pour un acteur tiers sans autorisation client" $ do
                myPointId <- testPointId
                shouldConsulterProd myPointId False

        describe nonRecevablesC $ do
            it "ADP-NR1 - Accès aux données d’un point inexistant" $ do
                myType <- initType pointIdUnkown True
                rep <- wsRequest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)

                rep `shouldHaveCode` "SGT401" -- Demande non recevable : point inexistant

    describe homologationC $ do
        describe recevablesC $ do
            it "ADP-R1 C5    - Accès aux données d’un point avec autorisation client" $
                shouldConsulterHomo adpPrmC5   True
            it "ADP-R1 C1-C4 - Accès aux données d’un point avec autorisation client" $
                shouldConsulterHomo adpPrmC1C4 True
            it "ADP-R2 C5    - Accès aux données d’un point sans autorisation client" $
                shouldConsulterHomo adpPrmC5   False
            it "ADP-R2 C1-C4 - Accès aux données d’un point sans autorisation client" $
                shouldConsulterHomo adpPrmC1C4 False

        describe nonRecevablesC $ do
            it "ADP-NR1 - Accès aux données d’un point inexistant (SGT401)" $
                pendingOnNetworkError $ do
                    myType <- initTypeTest pointIdUnkown True
                    rep    <- wsRequestTest myType
                                  :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
                    rep `shouldHaveCode` "SGT401"

main :: IO ()
main = hspec spec
