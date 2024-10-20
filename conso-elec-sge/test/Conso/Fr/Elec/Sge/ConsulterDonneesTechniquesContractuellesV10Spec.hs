module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Spec where

import SpecHelper
    ( Expectation, Spec, shouldSatisfy, describe, it, shouldBe, hspec )
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
    ( ConsulterDonneesTechniquesContractuellesResponseType )
import Conso.Fr.Elec.Sge.Sge ( wsRequestTest )
import Data.Either (isRight)


shouldConsulter :: String -> Bool -> Expectation
shouldConsulter myPointId auth = do
    myType <- initTypeTest myPointId auth
    rep <- wsRequestTest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    let pointIdC5 =     "25946599093143"
    let pointIdC1C4 =   "98800007059999"
    let pointIdUnkown = "99999999999999"

    describe "Demandes recevables" $ do
        it "ADP-R1 C5 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
            shouldConsulter pointIdC5 True

        --it "ADP-R1 C1C4 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
        --    shouldConsulter pointIdC1C4 True

        it "ADP-R2 C5 - Accès aux données d’un point en service pour un acteur tiers sans autorisation client" $ do
            shouldConsulter pointIdC5 False

        --it "ADP-R2 C1C4 - Accès aux données d’un point en service pour un acteur tiers sans autorisation client" $ do
        --    shouldConsulter pointIdC1C4 False

    --describe "Demandes non recevables" $ do
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
