module Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Spec where

import SpecHelper
import TestData (sadPrmC5R1)

import Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10
    ( initTypeTest )
import Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
    ( RechercherServicesAccesDonneesReponseType )
import Data.Either (isRight)


shouldRechercherHomo :: String -> Expectation
shouldRechercherHomo prm = pendingOnNetworkError $ do
    myType <- initTypeTest prm
    rep    <- wsRequestTest myType
                :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
    rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "RSA-R1 C5 - Recherche des services d'accès sur un PRM" $
                shouldRechercherHomo sadPrmC5R1
            -- RSA-R1 C2-C4 : non testable en homologation
            --   (doc p.39 : le WS ne fonctionne pas sur C2-C4 en homo)


main :: IO ()
main = hspec spec
