module Conso.Fr.Elec.Sge.ConsulterMesuresV11Spec where

import SpecHelper
import Conso.Fr.Elec.Sge.ConsulterMesuresV11
import Data.Either (isRight)
import           Text.Pretty.Simple (pPrint)


shouldConsulter :: String -> Bool -> Expectation
shouldConsulter pointId auth = do
    myType <- initType False pointId auth
    rep <- wsRequest False myType

    print $ "-----" ++ pointId
    pPrint myType
    pPrint rep
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    let pointIdC5 = "25957452924301"
    let pointIdC1C4 = "30001610071843"

    describe "Demandes recevables" $ do
        it "AHC-R1 C5 - Accès à l’historique de consommations pour un acteur tiers avec une autorisation client" $ do
            shouldConsulter pointIdC5 True 

        it "AHC-R1 C1C4 - Accès à l’historique de consommations pour un acteur tiers avec une autorisation client" $ do
            shouldConsulter pointIdC1C4 True 

    describe "Demandes non recevables" $ do
        it "AHC-NR1 Accès à l’historique de consommations pour un acteur tiers sans autorisation client" $ do
            myType <- initType False pointIdC5 False
            rep <- wsRequest False myType
            let (e, _) = case rep of
                            Left r -> r
                            Right _ -> ("to", "ti") 
            e `shouldBe` "SGT4G2" -- Le demandeur n'est pas éligible à la consultation des données de mesures sur le point


main :: IO ()
main = hspec spec
