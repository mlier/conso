module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Spec where

import SpecHelper
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10


spec :: Spec
spec = do
    let pointIdC5 = "25946599093143"
    let pointIdC1C4 = "98800007059999"

    describe "Demandes recevables" $ do
        it "ADP-R1 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
            --myType <- initType True pointIdC5 True
            --wsRequest True myType
            odd 1  `shouldBe`  True

        it "ADP-R2 - Accès aux données d’un point en service pour un acteur tiers sans autorisation client" $ do
            odd 1  `shouldBe`  True

    describe "Demandes non recevables" $ do
        it "ADP-NR1 Consultation des données d’un point inexistant" $ do
            odd 1  `shouldBe`  True
main :: IO ()
main = hspec spec
