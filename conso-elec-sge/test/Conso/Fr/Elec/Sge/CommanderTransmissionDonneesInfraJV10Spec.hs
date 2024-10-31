module Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Spec where

import SpecHelper   

import Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10
    ( xmlRequestTest,
      initTypeTest,
      AccordPersonneType(AccordPersonnePhysiqueNom),
      Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Type
    ( CommanderTransmissionDonneesInfraJResponseType )
import           Text.Pretty.Simple (pPrint)
import           Data.Either (isRight)



shouldDemanderTransHomo :: String -> Expectation
shouldDemanderTransHomo myPointId = do
    myType <- initTypeTest myPointId (AccordPersonnePhysiqueNom "Toto") SensSOUTIRAGE False True False
    
    rep <- wsRequestTest myType :: IO (Either (String, String) CommanderTransmissionDonneesInfraJResponseType)
    --rep <- xmlRequestTest myType
    --pPrint myType
    --pPrint rep
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    let myPointId = "98800000000246"

    describe homologationC $ do
        describe recevablesC $ do
            it "F375A-R1 - Demande de transmission de données infra-journalières avec autorisation client" $ do
                shouldDemanderTransHomo myPointId 
              


main :: IO ()
main = hspec spec
