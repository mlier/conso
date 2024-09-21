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
    ( RequestType,
      ResponseType,
      Env(test, sge),
      Sge(userB2b),
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(pointId),
      getEnv,
      sgeRequest )


instance RequestType ConsulterDonneesTechniquesContractuellesType
instance ResponseType ConsulterDonneesTechniquesContractuellesResponseType


initType :: String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initType myPointId autorisationClient = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv

    let requestType = ConsulterDonneesTechniquesContractuellesType{
          consulterDonneesTechniquesContractuellesType_pointId = PointIdType $ XsdString myPointId
        , consulterDonneesTechniquesContractuellesType_loginUtilisateur =  Ds.AdresseEmailType $ XsdString $ T.unpack loginUtilisateur
        , consulterDonneesTechniquesContractuellesType_autorisationClient = Just $ Ds.BooleenType autorisationClient
        }
    return requestType


wsRequest :: ConsulterDonneesTechniquesContractuellesType -> IO ()
wsRequest r = sgeRequest r configWS
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
    myType <- initType myPointId False
    wsRequest myType
