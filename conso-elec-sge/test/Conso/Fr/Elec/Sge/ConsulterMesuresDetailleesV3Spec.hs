module Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3Spec where

import SpecHelper
import TestData
    ( cmd3PrmC5, cmd3PrmC1C4a, cmd3PrmC1C4b
    , cmd3DateDebut, cmd3DateFin
    , cmd3DateDebutC1C4, cmd3DateFinC1C4
    , cmd3DateFinNR1 )

import Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3
    ( initTypeTest )
import Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
    ( ConsulterMesuresDetailleesV3ResponseType
    , MesuresTypeCodeType(..)
    , MesuresPasType(..)
    , SensMesureType(..)
    , CadreAccesType(..) )
import           Data.Either (isRight)
import qualified Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10 as ACCES
import           Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10
    ( AccordPersonneType(AccordPersonnePhysiqueNom)
    , Sens(SensSOUTIRAGE) )
import           Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( CommanderAccesDonneesMesuresResponseType )


shouldConsulterHomo
    :: String -> MesuresTypeCodeType -> String -> String -> String
    -> Maybe MesuresPasType -> Bool -> SensMesureType -> CadreAccesType
    -> Expectation
shouldConsulterHomo prm typeCode grandeur debut fin maybePas corrigees sens cadre =
    pendingOnNetworkError $ do
        myType <- initTypeTest prm typeCode grandeur debut fin maybePas corrigees sens cadre
        rep    <- wsRequestTest myType :: IO (Either (String, String) ConsulterMesuresDetailleesV3ResponseType)
        rep `shouldSatisfy` isRight

shouldRefuserHomo
    :: String -> MesuresTypeCodeType -> String -> String -> String
    -> Maybe MesuresPasType -> Bool -> SensMesureType -> CadreAccesType
    -> String -> Expectation
shouldRefuserHomo prm typeCode grandeur debut fin maybePas corrigees sens cadre expectedCode =
    pendingOnNetworkError $ do
        myType <- initTypeTest prm typeCode grandeur debut fin maybePas corrigees sens cadre
        rep    <- wsRequestTest myType :: IO (Either (String, String) ConsulterMesuresDetailleesV3ResponseType)
        rep `shouldHaveCode` expectedCode

-- | Variante pour CadreAccesTypeSERVICEACCES.
--   Active d'abord un service ACCES (ENERGIE 3 ans), puis exécute l'appel CMD3.
shouldConsulterAvecAccesHomo
    :: String -> MesuresTypeCodeType -> String -> String -> String
    -> Maybe MesuresPasType -> Bool -> SensMesureType -> Expectation
shouldConsulterAvecAccesHomo prm typeCode grandeur debut fin maybePas corrigees sens =
    pendingOnNetworkError $ do
        cleanupServices prm
        accesType <- ACCES.initTypeTest prm (Just (3 * 365))
                         (Just (AccordPersonnePhysiqueNom "Toto")) "ENERGIE" SensSOUTIRAGE
        _         <- wsRequestTest accesType
                         :: IO (Either (String, String) CommanderAccesDonneesMesuresResponseType)
        myType    <- initTypeTest prm typeCode grandeur debut fin maybePas corrigees sens
                         CadreAccesTypeSERVICEACCES
        rep       <- wsRequestTest myType
                         :: IO (Either (String, String) ConsulterMesuresDetailleesV3ResponseType)
        rep `shouldSatisfy` isRight


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "CMD3-R1 C5 - Courbe de puissance active (PA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypeCOURBE "PA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R1 C1-C4 - Courbe de puissance active (PA) C1-C4 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC1C4b MesuresTypeCodeTypeCOURBE "PA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R2 C1-C4 - Courbe de puissance réactive (PRI) C1-C4 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC1C4a MesuresTypeCodeTypeCOURBE "PRI"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R3 C5 - Énergie globale quotidienne(EA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypeENERGIE "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R3 C1-C4 - Énergie globale quotidienne (EA) C1-C4 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC1C4a MesuresTypeCodeTypeENERGIE "EA"
                    cmd3DateDebutC1C4 cmd3DateFinC1C4
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R4 C5 - Puissance maximale quotidienne (PMA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypePMAX "PMA"
                    cmd3DateDebut cmd3DateFin
                    (Just MesuresPasType_P1D) False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R5 C5 - Index (EA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypeINDEX "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R5 C1-C4 - Index (EA) C1-C4 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC1C4b MesuresTypeCodeTypeINDEX "EA"
                    cmd3DateDebutC1C4 cmd3DateFinC1C4
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R6 C5    - Accès via service d'accès (ENERGIE EA)" $ do
                shouldConsulterAvecAccesHomo
                    cmd3PrmC5 MesuresTypeCodeTypeENERGIE "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE

            it "CMD3-R6 C1-C4 - Accès via service d'accès (ENERGIE EA)" $ do
                shouldConsulterAvecAccesHomo
                    cmd3PrmC1C4b MesuresTypeCodeTypeENERGIE "EA"
                    cmd3DateDebutC1C4 cmd3DateFinC1C4
                    Nothing False SensMesureTypeSOUTIRAGE

        describe nonRecevablesC $ do
            it "CMD3-NR1 - Profondeur COURBE > 7 jours (SGT4L8)" $ do
                shouldRefuserHomo
                    cmd3PrmC5 MesuresTypeCodeTypeCOURBE "PA"
                    cmd3DateDebut cmd3DateFinNR1
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT
                    "SGT4L8"

            it "CMD3-NR2 - Aucun service souscrit, accès SERVICE_ACCES refusé (SGT211)" $ do
                shouldRefuserHomo
                    cmd3PrmC5 MesuresTypeCodeTypeINDEX "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeSERVICEACCES
                    "SGT211"


main :: IO ()
main = hspec spec
