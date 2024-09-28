{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10 where

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
    ( ResponseType,
      RequestType,
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(pointId),
      Env(test),
      getEnv,
      sgeRequest,
      getLoginContrat )
    

instance RequestType RechercherServicesSouscritsMesuresType
instance ResponseType RechercherServicesSouscritsMesuresResponseType


initType :: Bool -> String -> IO RechercherServicesSouscritsMesuresType
initType prod myPointId = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = RechercherServicesSouscritsMesuresType{ 
              rechercherServicesSouscritsMesuresType_criteres = CriteresType
                { criteresType_pointId = PointIdType $ XsdString myPointId
                , criteresType_contratId = ContratIdType $ XsdString contratId
                }
            , rechercherServicesSouscritsMesuresType_loginUtilisateur = Ds.AdresseEmailType $ XsdString loginUtilisateur
            }
    return requestType


wsRequest :: Bool -> RechercherServicesSouscritsMesuresType ->  
              IO ( Either (String, String) RechercherServicesSouscritsMesuresResponseType )
wsRequest prod r = sgeRequest prod r configWS
    where configWS = ConfigWS{
                          urlSge = "/RechercheServicesSouscritsMesures/v1.0"
                        , soapAction = "nimportequoimaispasvide"
                        , elementToXMLRequest = elementToXMLRechercherServicesSouscritsMesures
                        , xmlTag = "ns4:rechercherServicesSouscritsMesuresResponse" 
                        , elementResponse = elementRechercherServicesSouscritsMesuresResponse
    }

myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType True (T.unpack $ pointId testEnv)
    rep <- wsRequest True myType
    pPrint rep
