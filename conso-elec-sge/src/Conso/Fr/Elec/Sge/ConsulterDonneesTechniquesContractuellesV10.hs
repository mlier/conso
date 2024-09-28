{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10 where

import qualified Data.Text as T
import           Data.Maybe ( fromMaybe )
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType(PointIdType),
      BooleenType(BooleenType),
      AdresseEmailType(AdresseEmailType) )
import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
    ( elementConsulterDonneesTechniquesContractuellesResponse,
      elementToXMLConsulterDonneesTechniquesContractuelles,
      ConsulterDonneesTechniquesContractuellesResponseType,
      ConsulterDonneesTechniquesContractuellesType(..) )
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


instance RequestType ConsulterDonneesTechniquesContractuellesType
instance ResponseType ConsulterDonneesTechniquesContractuellesResponseType


initType :: Bool -> String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initType prod myPointId autorisationClient = do
    (loginUtilisateur, _) <- getLoginContrat prod

    let requestType = ConsulterDonneesTechniquesContractuellesType{
          consulterDonneesTechniquesContractuellesType_pointId = PointIdType $ XsdString myPointId
        , consulterDonneesTechniquesContractuellesType_loginUtilisateur =  Ds.AdresseEmailType $ XsdString loginUtilisateur
        , consulterDonneesTechniquesContractuellesType_autorisationClient = Just $ Ds.BooleenType autorisationClient
        }
    return requestType


wsRequest :: Bool -> ConsulterDonneesTechniquesContractuellesType -> IO ()
wsRequest prod r = sgeRequest prod r configWS
    where configWS = ConfigWS{
                  urlSge = "/ConsultationDonneesTechniquesContractuelles/v1.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLConsulterDonneesTechniquesContractuelles
                , xmlTag = "ns7:consulterDonneesTechniquesContractuellesResponse"
                , elementResponse = elementConsulterDonneesTechniquesContractuellesResponse
}


myrequest :: Maybe String -> IO()
myrequest mPointId = do
    env <- getEnv
    let testEnv = test env
    let myPointId = fromMaybe (T.unpack $ pointId testEnv) mPointId
    myType <- initType True myPointId False
    wsRequest True myType
