module Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Spec where

import SpecHelper
import TestData (sadPrmC5R3)
import Data.Maybe (listToMaybe)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import qualified Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10 as SAD
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
    ( CommanderServicesAccesDonneesResponseType
    , commanderServicesAccesDonneesResponseType_affaires
    , affairesType_affaire, affaireType_serviceSouscritMesures
    , serviceSouscritMesuresType_serviceSouscritId
    , ServiceIdType(ServiceIdType) )

import Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10
    ( initTypeTest, Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
    ( CommanderArretServicesAccesDonneesResponseType )
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
                 (Just (SAD.AccordPersonnePhysiqueNom "Toto")) "IDX" (Just 500)
    rep     <- wsRequestTest sadType
                 :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    case rep of
        Left ("SGT570", _) -> return Nothing
        Left (code, label) -> expectationFailure
            ("SAD a échoué : " ++ code ++ " (" ++ label ++ ")") >> return Nothing
        Right r            -> return (extractServiceId r)

shouldArretHomo :: String -> Expectation
shouldArretHomo prm = pendingOnNetworkError $ do
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            myType <- initTypeTest prm SensSOUTIRAGE [sid]
            rep    <- wsRequestTest myType
                         :: IO (Either (String, String) CommanderArretServicesAccesDonneesResponseType)
            rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "ASAD-R1 C5 - Arrêt d'un service d'accès souscrit en soutirage" $
                shouldArretHomo sadPrmC5R3


main :: IO ()
main = hspec spec
