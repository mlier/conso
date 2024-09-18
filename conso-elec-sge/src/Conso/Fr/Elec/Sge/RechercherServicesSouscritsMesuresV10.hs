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
    ( RequestType,
      ResponseType,
      Env(test, sge),
      Sge(contractId, userB2b),
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(pointId),
      getEnv,
      sgeRequest )
    

instance RequestType RechercherServicesSouscritsMesuresType
instance ResponseType RechercherServicesSouscritsMesuresResponseType


initType :: String -> IO RechercherServicesSouscritsMesuresType
initType myPointId = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv
    let contratId = contractId sgeEnv

    let requestType = RechercherServicesSouscritsMesuresType{ 
              rechercherServicesSouscritsMesuresType_criteres = CriteresType
                { criteresType_pointId = PointIdType $ XsdString myPointId
                , criteresType_contratId = ContratIdType $ XsdString $ T.unpack contratId
                }
            , rechercherServicesSouscritsMesuresType_loginUtilisateur = Ds.AdresseEmailType $ XsdString $ T.unpack loginUtilisateur
            }
    return requestType


wsRequest :: RechercherServicesSouscritsMesuresType -> IO ()
wsRequest r = sgeRequest r configWS
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
    myType <- initType (T.unpack $ pointId testEnv)
    wsRequest myType
    pPrint myType
