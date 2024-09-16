{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ConsulterMesuresDetailleesV3 where

import           Data.Text ( Text )
import qualified Data.Text as T
--import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), Boolean )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.XML.HaXml.Schema.Schema as Schema ( Content, XMLParser )
import           Text.Pretty.Simple (pPrint)

import           EnedisDictionnaireTypeSimpleV50 as Ds
import           ConsulterMesuresDetailleesCommunV12Type
import Sge


instance RequestType ConsulterMesuresDetailleesV3Type
instance ResponseType ConsulterMesuresDetailleesV3ResponseType
               

initType :: String -> MesuresTypeCodeType -> String -> String -> String -> Maybe MesuresPasType -> 
            Bool -> SensMesureType -> CadreAccesType -> IO ConsulterMesuresDetailleesV3Type
initType pointId mesuresTypeCode grandeurPhysique dateDebut dateFin 
         mesuresPas mesuresCorrigees sens cadreAcces = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv

    let requestType = ConsulterMesuresDetailleesV3Type{ 
          consulterMesuresDetailleesV3Type_demande = Demande {
                  demande_initiateurLogin = Xsd.XsdString $ T.unpack loginUtilisateur
                , demande_pointId = PointIdType $ Xsd.XsdString pointId
                , demande_mesuresTypeCode = mesuresTypeCode
                , demande_grandeurPhysique = Xsd.XsdString grandeurPhysique
                , demande_dateDebut = Xsd.Date dateDebut
                , demande_dateFin = Xsd.Date dateFin
                , demande_mesuresPas = mesuresPas
                , demande_mesuresCorrigees = mesuresCorrigees
                , demande_sens = sens
                , demande_cadreAcces = cadreAcces
          }
        }
    return requestType


wsRequest :: ConsulterMesuresDetailleesV3Type -> IO ()
wsRequest r = sgeRequest r configWS
    where configWS = ConfigWS{
                  urlSge = "/ConsultationMesuresDetaillees/v3.0"
                , soapAction = "http://www.enedis.fr/sge/b2b/services/consultationmesuresdetaillees/v3.0"
                , elementToXMLRequest = elementToXMLConsulterMesuresDetailleesV3
                , xmlTag = "ns4:consulterMesuresDetailleesResponseV3"
                , elementResponse = elementConsulterMesuresDetailleesResponseV3
}  


myrequest :: IO()
myrequest = do 
    myType <- initType "21429667044956" MesuresTypeCodeType_INDEX "EA" "2024-08-01" "2024-09-01" 
                       Nothing False SensMesureType_SOUTIRAGE CadreAccesType_ACCORD_CLIENT
    wsRequest myType
