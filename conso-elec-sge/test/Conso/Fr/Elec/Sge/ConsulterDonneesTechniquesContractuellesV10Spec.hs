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
import TestData (adpPrmC5)


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
    --let pointIdC1C4 =   "98800007059999" -- ne fonctionne pas
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
            it "ADP-R1 C5 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
                shouldConsulterHomo adpPrmC5 True

            --it "ADP-R1 C1C4 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
            --    shouldConsulter pointIdC1C4 True

            it "ADP-R2 C5 - Accès aux données d’un point en service pour un acteur tiers sans autorisation client" $ do
                shouldConsulterHomo adpPrmC5 False

            --it "ADP-R2 C1C4 - Accès aux données d’un point en service pour un acteur tiers sans autorisation client" $ do
            --    shouldConsulter pointIdC1C4 False

        --describe (redC "NON RECEVABLES") $ do
        --    it "ADP-NR1 - Accès aux données d’un point inexistant" $ do
        --        myType <- initTypeTest pointIdUnkown True
        --        rep <- wsRequestTest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
        --        
        --        let (e, _) = case rep of
        --                        Left r -> r
        --                        Right _ -> ("toto", "titi") 
        --        e `shouldBe` "SGT401" -- Demande non recevable : point inexistant

main :: IO ()
main = hspec spec
