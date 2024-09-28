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
      Env(test),
      getEnv,
      sgeRequest,
      getLoginContrat )


import qualified Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Xsd


instance RequestType ConsulterMesuresType 
instance ResponseType ConsulterMesuresResponseType
               

initType :: Bool -> String -> IO ConsulterMesuresType
initType prod myPointId= do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = ConsulterMesuresType
            { consulterMesuresType_pointId = PointIdType $ Xsd.XsdString myPointId
            , consulterMesuresType_loginDemandeur = AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , consulterMesuresType_contratId = ContratIdType $ Xsd.XsdString contratId
            , consulterMesuresType_choice3 = Just ( TwoOf2 $ Xsd.BooleenType True )
            }
    return requestType


wsRequest :: Bool -> ConsulterMesuresType -> IO ()
wsRequest prod r = sgeRequest prod r configWS
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
    myType <- initType True (T.unpack $ pointId testEnv)
    wsRequest True myType
