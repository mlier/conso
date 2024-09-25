module Conso.Fr.Elec.Sge.ConsulterMesuresV11Spec where

import SpecHelper
import Conso.Fr.Elec.Sge.ConsulterMesuresV11


spec :: Spec
spec = do
    let pointIdC5 = "25957452924301"
    let pointIdC1C4 = "30001610071843"

    describe "Demandes recevables" $ do
        it "AHC-R1 Accès à l’historique de consommations pour un acteur tiers avec une autorisation client" $ do
            odd 1  `shouldBe`  True

    describe "Demandes non recevables" $ do
        it "AHC-NR1 Accès à l’historique de consommations pour un acteur tiers sans autorisation client" $ do
            odd 1  `shouldBe`  True
main :: IO ()
main = hspec spec
