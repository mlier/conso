module Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Spec where

import SpecHelper
import TestData (sadPrmC5R2, mosadPeriodicite)
import Data.Maybe (listToMaybe)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import qualified Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10 as SAD
import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
    ( CommanderServicesAccesDonneesResponseType
    , commanderServicesAccesDonneesResponseType_affaires
    , affairesType_affaire, affaireType_serviceSouscritMesures
    , serviceSouscritMesuresType_serviceSouscritId
    , ServiceIdType(ServiceIdType) )

import qualified Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10 as ASAD
import Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
    ( CommanderArretServicesAccesDonneesResponseType )

import Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10
    ( initTypeTest, Sens(SensSOUTIRAGE) )
import Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Type
    ( CommanderModificationOptionsServicesAccesDonneesResponseType )
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
                 (Just (SAD.AccordPersonnePhysiqueNom "Toto")) "ENERGIE" Nothing
    rep     <- wsRequestTest sadType
                 :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    case rep of
        Left ("SGT570", _) -> return Nothing
        Left (code, label) -> expectationFailure
            ("SAD a échoué : " ++ code ++ " (" ++ label ++ ")") >> return Nothing
        Right r            -> return (extractServiceId r)

-- | Arrête un service existant (nettoyage après test).
stopService :: String -> String -> IO ()
stopService prm sid = do
    asadType <- ASAD.initTypeTest prm ASAD.SensSOUTIRAGE [sid]
    rep      <- wsRequestTest asadType
                  :: IO (Either (String, String) CommanderArretServicesAccesDonneesResponseType)
    case rep of
        Left (code, label) -> expectationFailure
            $ "ASAD cleanup a échoué : " ++ code ++ " (" ++ label ++ ")"
        Right _ -> return ()

shouldAjouterOptionsHomo :: String -> Expectation
shouldAjouterOptionsHomo prm = pendingOnNetworkError $ do
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            myType <- initTypeTest prm SensSOUTIRAGE sid
                         [(Nothing, mosadPeriodicite)] []
            rep    <- wsRequestTest myType
                         :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            rep `shouldSatisfy` isRight
            stopService prm sid

shouldSupprimerOptionsHomo :: String -> Expectation
shouldSupprimerOptionsHomo prm = pendingOnNetworkError $ do
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            -- D'abord ajouter une option, puis la supprimer
            addType <- initTypeTest prm SensSOUTIRAGE sid
                          [(Nothing, mosadPeriodicite)] []
            addRep  <- wsRequestTest addType
                          :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            addRep `shouldSatisfy` isRight
            delType <- initTypeTest prm SensSOUTIRAGE sid
                          [] [(Nothing, mosadPeriodicite)]
            delRep  <- wsRequestTest delType
                          :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            delRep `shouldSatisfy` isRight
            stopService prm sid


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "MOSAD-R1 C5 - Ajout d'une option de publication sur un service d'accès" $
                shouldAjouterOptionsHomo sadPrmC5R2
            it "MOSAD-R2 C5 - Suppression d'une option de publication sur un service d'accès" $
                shouldSupprimerOptionsHomo sadPrmC5R2


main :: IO ()
main = hspec spec
