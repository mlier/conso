module Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Spec where

import SpecHelper
import TestData (sadPrmC5R2, sadPrmC2C4)
import Data.Maybe (listToMaybe)
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import qualified Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10 as SAD
    ( initTypeTest
    , AccordPersonneType(AccordPersonnePhysiqueNom, AccordPersonneMoraleDenominationSociale)
    , Sens(SensSOUTIRAGE) )
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
    ( initTypeTest, Sens(SensSOUTIRAGE), Periodicite(P1D) )
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
                 (Just (SAD.AccordPersonnePhysiqueNom "Toto")) "IDX" (Just 500)
    rep     <- wsRequestTest sadType
                 :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
    case rep of
        Left ("SGT570", _) -> return Nothing
        Left (code, label) -> expectationFailure
            ("SAD a échoué : " ++ code ++ " (" ++ label ++ ")") >> return Nothing
        Right r            -> return (extractServiceId r)

-- | Crée un service IDX via SAD pour un segment C2-C4, retourne son serviceId.
--   Nothing si SGT570 (service déjà actif, ID inconnu).
createServiceC2C4 :: String -> IO (Maybe String)
createServiceC2C4 prm = do
    sadType <- SAD.initTypeTest prm SAD.SensSOUTIRAGE
                 (Just (SAD.AccordPersonneMoraleDenominationSociale "Toto")) "IDX" (Just 500)
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
    cleanupServices prm
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            myType <- initTypeTest prm SensSOUTIRAGE sid
                         [(Nothing, P1D)] []
            rep    <- wsRequestTest myType
                         :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            rep `shouldSatisfy` isRight
            stopService prm sid

shouldAjouterOptionsC2C4Homo :: String -> Expectation
shouldAjouterOptionsC2C4Homo prm = pendingOnNetworkError $ do
    cleanupServices prm
    msid <- createServiceC2C4 prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            myType <- initTypeTest prm SensSOUTIRAGE sid
                         [(Nothing, P1D)] []
            rep    <- wsRequestTest myType
                         :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            rep `shouldSatisfy` isRight
            stopService prm sid

shouldSupprimerOptionsC2C4Homo :: String -> Expectation
shouldSupprimerOptionsC2C4Homo prm = pendingOnNetworkError $ do
    cleanupServices prm
    msid <- createServiceC2C4 prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            addType <- initTypeTest prm SensSOUTIRAGE sid
                          [(Nothing, P1D)] []
            addRep  <- wsRequestTest addType
                          :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            addRep `shouldSatisfy` isRight
            delType <- initTypeTest prm SensSOUTIRAGE sid
                          [] [(Nothing, P1D)]
            delRep  <- wsRequestTest delType
                          :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            delRep `shouldSatisfy` isRight
            stopService prm sid

shouldSupprimerOptionsHomo :: String -> Expectation
shouldSupprimerOptionsHomo prm = pendingOnNetworkError $ do
    cleanupServices prm
    msid <- createService prm
    case msid of
        Nothing  -> pendingWith "Service déjà actif (SGT570) : serviceId inconnu"
        Just sid -> do
            -- D'abord ajouter une option, puis la supprimer
            addType <- initTypeTest prm SensSOUTIRAGE sid
                          [(Nothing, P1D)] []
            addRep  <- wsRequestTest addType
                          :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
            addRep `shouldSatisfy` isRight
            delType <- initTypeTest prm SensSOUTIRAGE sid
                          [] [(Nothing, P1D)]
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
            it "MOSAD-R1 C2-C4 - Ajout d'une option de publication sur un service d'accès" $
                shouldAjouterOptionsC2C4Homo sadPrmC2C4
            it "MOSAD-R2 C2-C4 - Suppression d'une option de publication sur un service d'accès" $
                shouldSupprimerOptionsC2C4Homo sadPrmC2C4


main :: IO ()
main = hspec spec
