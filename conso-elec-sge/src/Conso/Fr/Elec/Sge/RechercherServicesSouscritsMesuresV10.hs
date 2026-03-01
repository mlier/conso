{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString) )
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType(PointIdType),
      ContratIdType(ContratIdType),
      AdresseEmailType(AdresseEmailType) )
import Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type
    ( elementRechercherServicesSouscritsMesuresResponse,
      elementToXMLRechercherServicesSouscritsMesures,
      CriteresType(CriteresType, criteresType_contratId,
                   criteresType_pointId),
      RechercherServicesSouscritsMesuresResponseType,
      RechercherServicesSouscritsMesuresType(..) )
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
 
    

instance RequestType RechercherServicesSouscritsMesuresType where
  configReq = ConfigRequest{
                     urlSge = "/RechercheServicesSouscritsMesures/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLRechercherServicesSouscritsMesures
                   }

instance ResponseType RechercherServicesSouscritsMesuresResponseType where
  configResp = ConfigResponse{
                     xmlTag = "rechercherServicesSouscritsMesuresResponse" 
                   , elementResponse = elementRechercherServicesSouscritsMesuresResponse
                   }


initType_ :: Bool -> String -> IO RechercherServicesSouscritsMesuresType
initType_ prod myPointId = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = RechercherServicesSouscritsMesuresType{ 
              rechercherServicesSouscritsMesuresType_criteres = CriteresType
                { criteresType_pointId = PointIdType $ XsdString myPointId
                , criteresType_contratId = ContratIdType $ XsdString contratId
                }
            , rechercherServicesSouscritsMesuresType_loginUtilisateur = Ds.AdresseEmailType $ XsdString loginUtilisateur
            }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String    -- ^ myPointId : point de référence sur lequel on souhaite obtenir des informations.
        -> IO RechercherServicesSouscritsMesuresType
initType = initType_ True

initTypeTest :: String -> IO RechercherServicesSouscritsMesuresType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    rep <- wsRequest myType :: IO ( Either (String, String) RechercherServicesSouscritsMesuresResponseType )
    pPrint rep
