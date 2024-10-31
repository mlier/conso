module Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Spec where

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
      recevablesC )

import Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10
    ( initType, initTypeTest )
import Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type
    ( RechercherServicesSouscritsMesuresResponseType )
import           Data.Either (isRight)


shouldConsulterProd :: String -> Expectation
shouldConsulterProd myPointId = do
    myType <- initType myPointId 
    rep <- wsRequest myType :: IO (Either (String, String) RechercherServicesSouscritsMesuresResponseType)
    rep `shouldSatisfy` isRight

shouldConsulterHomo :: String -> Expectation
shouldConsulterHomo myPointId = do
    myType <- initTypeTest myPointId
    rep <- wsRequestTest myType :: IO (Either (String, String) RechercherServicesSouscritsMesuresResponseType)
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    let pointIdC5 =     "25884515170669"
    let pointIdC1C4 =   "98800000000246"


    describe productionC $ do
        describe recevablesC $ do
            it "RS-R1 C5 - Recherche des services souscrits sur un PRM" $ do
                myPointId <- testPointId
                shouldConsulterProd myPointId 

    describe homologationC $ do
        describe recevablesC $ do
            it "ADP-R1 C5 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
                shouldConsulterHomo pointIdC5 

            it "ADP-R1 C1C4 - Accès aux données d’un point pour un acteur tiers avec une autorisation client" $ do
                shouldConsulterHomo pointIdC1C4 

main :: IO ()
main = hspec spec
