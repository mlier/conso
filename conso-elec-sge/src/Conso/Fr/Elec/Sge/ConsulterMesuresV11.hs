{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresV11 where

import qualified Data.Text as T
import           Text.XML.HaXml.OneOfN ( OneOf2(TwoOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( AdresseEmailType(AdresseEmailType),
      ContratIdType(ContratIdType),
      PointIdType(PointIdType) )
import Conso.Fr.Elec.Sge.ConsulterMesuresV11Type
    ( ConsulterMesuresResponseType,
      ConsulterMesuresType(..),
      elementToXMLConsulterMesures,
      elementConsulterMesuresResponse )
import Conso.Fr.Elec.Sge.Sge
    ( ResponseType,
      RequestType,
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(pointId),
      Sge(contractId, userB2b),
      Env(test, sge),
      getEnv,
      sgeRequest )

import qualified Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Xsd


instance RequestType ConsulterMesuresType 
instance ResponseType ConsulterMesuresResponseType
               

initType :: Sge -> String -> IO ConsulterMesuresType
initType envSge myPointId= do
    let loginUtilisateur = userB2b envSge
    let contratId = contractId envSge

    let requestType = ConsulterMesuresType
            { consulterMesuresType_pointId = PointIdType $ Xsd.XsdString myPointId
            , consulterMesuresType_loginDemandeur = AdresseEmailType $ Xsd.XsdString $ T.unpack loginUtilisateur
            , consulterMesuresType_contratId = ContratIdType $ Xsd.XsdString $ T.unpack contratId
            , consulterMesuresType_choice3 = Just ( TwoOf2 $ Xsd.BooleenType True )
            }
    return requestType


wsRequest :: Sge -> ConsulterMesuresType -> IO ()
wsRequest envSge r = sgeRequest envSge r configWS
    where configWS = ConfigWS{
                  urlSge = "/ConsultationMesures/v1.1"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLConsulterMesures
                , xmlTag = "ns4:consulterMesuresResponse"
                , elementResponse = elementConsulterMesuresResponse
}  


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    let envSge = sge env
    myType <- initType envSge (T.unpack $ pointId testEnv)
    wsRequest envSge myType
