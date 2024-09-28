{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType(PointIdType) )

import Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
    ( elementConsulterMesuresDetailleesResponseV3,
      elementToXMLConsulterMesuresDetailleesV3,
      CadreAccesType(CadreAccesTypeACCORDCLIENT),
      ConsulterMesuresDetailleesV3ResponseType,
      ConsulterMesuresDetailleesV3Type(..),
      Demande(Demande, demande_cadreAcces, demande_initiateurLogin,
              demande_pointId, demande_mesuresTypeCode, demande_grandeurPhysique,
              demande_dateDebut, demande_dateFin, demande_mesuresPas,
              demande_mesuresCorrigees, demande_sens),
      MesuresPasType,
      MesuresTypeCodeType(MesuresTypeCodeTypeINDEX),
      SensMesureType(SensMesureTypeSOUTIRAGE) )

import Conso.Fr.Elec.Sge.Sge
    ( ResponseType,
      RequestType,
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      sgeRequest,
      getLoginContrat )


instance RequestType ConsulterMesuresDetailleesV3Type
instance ResponseType ConsulterMesuresDetailleesV3ResponseType
               

initType :: Bool -> String -> MesuresTypeCodeType -> String -> String -> String -> Maybe MesuresPasType -> 
            Bool -> SensMesureType -> CadreAccesType -> IO ConsulterMesuresDetailleesV3Type
initType prod myPointId mesuresTypeCode grandeurPhysique dateDebut dateFin 
         mesuresPas mesuresCorrigees sens cadreAcces = do
    (loginUtilisateur, _) <- getLoginContrat prod

    let requestType = ConsulterMesuresDetailleesV3Type{ 
          consulterMesuresDetailleesV3Type_demande = Demande {
                  demande_initiateurLogin = Xsd.XsdString loginUtilisateur
                , demande_pointId = PointIdType $ Xsd.XsdString myPointId
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


wsRequest :: Bool -> ConsulterMesuresDetailleesV3Type -> 
              IO ( Either (String, String) ConsulterMesuresDetailleesV3ResponseType )
wsRequest prod r = sgeRequest prod r configWS
    where configWS = ConfigWS{
                  urlSge = "/ConsultationMesuresDetaillees/v3.0"
                , soapAction = "http://www.enedis.fr/sge/b2b/services/consultationmesuresdetaillees/v3.0"
                , elementToXMLRequest = elementToXMLConsulterMesuresDetailleesV3
                , xmlTag = "ns4:consulterMesuresDetailleesResponseV3"
                , elementResponse = elementConsulterMesuresDetailleesResponseV3
}  


myrequest :: IO()
myrequest = do 
    myType <- initType True "21429667044956" MesuresTypeCodeTypeINDEX "EA" "2024-08-01" "2024-09-01" 
                       Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT
    rep <- wsRequest True myType
    pPrint rep
