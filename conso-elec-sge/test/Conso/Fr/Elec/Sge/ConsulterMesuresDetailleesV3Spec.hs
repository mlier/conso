module Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3Spec where

import SpecHelper
import TestData
    ( cmd3PrmC5, cmd3PrmC1C4a
    , cmd3DateDebut, cmd3DateFin
    , cmd3DateDebutLong, cmd3DateFinLong )

import Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3
    ( initTypeTest )
import Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
    ( ConsulterMesuresDetailleesV3ResponseType
    , MesuresTypeCodeType(..)
    , MesuresPasType(..)
    , SensMesureType(..)
    , CadreAccesType(..) )
import           Data.Either (isRight, isLeft)


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
        rep `shouldSatisfy` isLeft
        case rep of
            Left (code, _) -> code `shouldBe` expectedCode
            Right _        -> expectationFailure "Réponse inattendue : Right"


spec :: Spec
spec = do
    describe homologationC $ do
        describe recevablesC $ do
            it "CMD3-R1 - Courbe de puissance (PA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypeCOURBE "PA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R2 - Énergie (EA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypeENERGIE "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R3 - Énergie (EA) C1-C4 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC1C4a MesuresTypeCodeTypeENERGIE "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R4 - Puissance maximale (PMA) C5 avec autorisation client (pas mensuel)" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypePMAX "PMA"
                    cmd3DateDebut cmd3DateFin
                    (Just MesuresPasType_P1M) False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

            it "CMD3-R5 - Index (EA) C5 avec autorisation client" $ do
                shouldConsulterHomo
                    cmd3PrmC5 MesuresTypeCodeTypeINDEX "EA"
                    cmd3DateDebut cmd3DateFin
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT

        describe nonRecevablesC $ do
            it "CMD3-NR1 - Période supérieure à 2 ans (SGT4G7)" $ do
                shouldRefuserHomo
                    cmd3PrmC5 MesuresTypeCodeTypeCOURBE "PA"
                    cmd3DateDebutLong cmd3DateFinLong
                    Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT
                    "SGT4G7"


main :: IO ()
main = hspec spec
