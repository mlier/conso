module Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Spec where

import SpecHelper
import TestData (sadPrmC5R1, sadPrmC2C4)
import Data.Maybe (listToMaybe)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import qualified Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10 as SAD
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
    ( CommanderServicesAccesDonneesResponseType
    , commanderServicesAccesDonneesResponseType_affaires
    , affairesType_affaire, affaireType_serviceSouscritMesures
    , serviceSouscritMesuresType_serviceSouscritId
    , ServiceIdType(ServiceIdType) )

import Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10
    ( initTypeTest
    , AccordPersonneType(AccordPersonnePhysiqueNom)
    , Sens(SensSOUTIRAGE, SensINJECTION) )
import Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
    ( RenouvelerServicesAccesResponseType )
import Data.Either (isRight)


extractServiceId :: CommanderServicesAccesDonneesResponseType -> Maybe String
extractServiceId resp = do
    affaires <- commanderServicesAccesDonneesResponseType_affaires resp
    affaire  <- listToMaybe $ affairesType_affaire affaires
    let ServiceIdType (Xsd.XsdString sid) =
            serviceSouscritMesuresType_serviceSouscritId
              (affaireType_serviceSouscritMesures affaire)
    return sid

-- | Crée un service ENERGIE via SAD, retourne son serviceId.
--   Nothing si SGT570 (service déjà actif, ID inconnu).
createService :: String -> IO (Maybe String)
createService prm = do
    sadType <- SAD.initTypeTest prm SAD.SensSOUTIRAGE
                 (Just (SAD.AccordPersonnePhysiqueNom "Toto")) "ENERGIE" (Just 10)
    rep     <- wsRequestTest sadType
                 :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    case rep of
        Left ("SGT570", _) -> return Nothing
        Left (code, label) -> expectationFailure
            ("SAD a échoué : " ++ code ++ " (" ++ label ++ ")") >> return Nothing
        Right r            -> return (extractServiceId r)

shouldRenouvelerHomo :: String -> Expectation
shouldRenouvelerHomo prm = pendingOnNetworkError $ do
    cleanupServices prm
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            myType <- initTypeTest prm SensSOUTIRAGE
                         (AccordPersonnePhysiqueNom "Toto") [sid] (Just 500)
            rep    <- wsRequestTest myType
                         :: IO (Either (String, String) RenouvelerServicesAccesResponseType)
            rep `shouldSatisfy` isRight

shouldRefuserRenouvelerHomo :: String -> Expectation
shouldRefuserRenouvelerHomo prm = pendingOnNetworkError $ do
    cleanupServices prm
    -- Crée un service SOUTIRAGE, tente de le renouveler en INJECTION → SGT4O3
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            myType <- initTypeTest prm SensINJECTION
                         (AccordPersonnePhysiqueNom "Toto") [sid] (Just 500)
            rep    <- wsRequestTest myType
                         :: IO (Either (String, String) RenouvelerServicesAccesResponseType)
            rep `shouldHaveCode` "SGT4O3"


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "RSAD-R1 C5 - Renouvellement d'un service d'accès en soutirage" $
                shouldRenouvelerHomo sadPrmC5R1
            it "RSAD-R1 C2-C4 - Renouvellement d'un service d'accès en soutirage" $
                shouldRenouvelerHomo sadPrmC2C4

        describe nonRecevablesC $ do
            it "RSAD-NR1 C5 - Renouvellement en injection sur un service d'accès en soutirage (SGT566)" $
                shouldRefuserRenouvelerHomo sadPrmC5R1
            it "RSAD-NR1 C2-C4 - Renouvellement en injection sur un service d'accès en soutirage (SGT566)" $
                shouldRefuserRenouvelerHomo sadPrmC2C4


main :: IO ()
main = hspec spec
