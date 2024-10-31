{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresV11 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

import qualified Data.Text as T
import           Text.XML.HaXml.OneOfN ( OneOf2(TwoOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( BooleenType(BooleenType),
      PointIdType(PointIdType),
      ContratIdType(ContratIdType),
      AdresseEmailType(AdresseEmailType) )

import Conso.Fr.Elec.Sge.ConsulterMesuresV11Type
    ( ConsulterMesuresType(..),
      elementToXMLConsulterMesures,
      ConsulterMesuresResponseType,
      elementConsulterMesuresResponse )

import Conso.Fr.Elec.Sge.Sge
    ( getEnv,
      getLoginContrat,
      wsRequest,
      wsRequestTest,
      xmlRequest,
      xmlRequestTest,
      ConfigRequest(ConfigRequest, elementToXMLRequest, urlSge,
                    soapAction),
      ConfigResponse(ConfigResponse, elementResponse, xmlTag),
      RequestType(..),
      ResponseType(..),
      SgeEnv(test),
      Test(pointId) )
  

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

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String  -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Bool    -- ^ autorisationClient : existence d’une autorisation du client actuel.
         -> IO ConsulterMesuresType
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
