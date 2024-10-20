{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresV11 where

import qualified Data.Text as T
import           Text.XML.HaXml.OneOfN ( OneOf2(TwoOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds

import Conso.Fr.Elec.Sge.ConsulterMesuresV11Type

import Conso.Fr.Elec.Sge.Sge
  

instance RequestType ConsulterMesuresType where
  configReq = ConfigRequest{
                     urlSge = "/ConsultationMesures/v1.1"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLConsulterMesures
                   }

instance ResponseType ConsulterMesuresResponseType where
  configResp = ConfigResponse{
                     xmlTag = "ns4:consulterMesuresResponse"
                   , elementResponse = elementConsulterMesuresResponse
                   }


initType_ :: Bool -> String -> Bool -> IO ConsulterMesuresType
initType_ prod myPointId autorisationClient = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = ConsulterMesuresType
            { consulterMesuresType_pointId = PointIdType $ Xsd.XsdString myPointId
            , consulterMesuresType_loginDemandeur = AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , consulterMesuresType_contratId = ContratIdType $ Xsd.XsdString contratId
            , consulterMesuresType_choice3 = Just ( TwoOf2 $ Ds.BooleenType autorisationClient )
            }
    return requestType

initType :: String -> Bool -> IO ConsulterMesuresType
initType = initType_ True

initTypeTest :: String -> Bool -> IO ConsulterMesuresType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) True
    rep <- wsRequest myType :: IO (Either (String, String) ConsulterMesuresResponseType)
    --rep <- xmlRequest myType
    pPrint rep
