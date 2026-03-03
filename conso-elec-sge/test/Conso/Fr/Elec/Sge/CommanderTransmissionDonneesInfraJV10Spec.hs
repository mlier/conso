module Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Spec where

import SpecHelper
import TestData (f375aPrmC2C4)

import Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10
    ( initTypeTest
    , AccordPersonneType(AccordPersonnePhysiqueNom)
    , Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Type
    ( CommanderTransmissionDonneesInfraJResponseType )
import           Data.Either (isRight)


shouldDemanderTransHomo :: String -> Expectation
shouldDemanderTransHomo myPointId = pendingOnNetworkError $ do
    myType <- initTypeTest myPointId (AccordPersonnePhysiqueNom "Toto") SensSOUTIRAGE False True False
    rep <- wsRequestTest myType :: IO (Either (String, String) CommanderTransmissionDonneesInfraJResponseType)
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "F375A-R1 - Demande de transmission de données infra-journalières avec autorisation client" $ do
                shouldDemanderTransHomo f375aPrmC2C4


main :: IO ()
main = hspec spec
