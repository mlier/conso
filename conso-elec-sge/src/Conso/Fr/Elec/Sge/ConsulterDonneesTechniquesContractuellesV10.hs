{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}

module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10 where

import qualified Data.Text as T
import           Data.Maybe ( fromMaybe )
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), )
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( BooleenType(BooleenType),
      PointIdType(PointIdType),
      AdresseEmailType(AdresseEmailType) )

import Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
    ( ConsulterDonneesTechniquesContractuellesType(..),
      ConsulterDonneesTechniquesContractuellesResponseType,
      elementToXMLConsulterDonneesTechniquesContractuelles,
      elementConsulterDonneesTechniquesContractuellesResponse )

import Conso.Fr.Elec.Sge.Sge
    ( RequestType(..),
      ResponseType(..),
      ConfigRequest(ConfigRequest, elementToXMLRequest, urlSge,
                    soapAction),
      ConfigResponse(ConfigResponse, elementResponse, xmlTag),
      getEnv,
      getLoginContrat,
      wsRequest,
      Env(test),
      Test(pointId) )


instance RequestType ConsulterDonneesTechniquesContractuellesType where
  configReq = ConfigRequest{
                     urlSge = "/ConsultationDonneesTechniquesContractuelles/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLConsulterDonneesTechniquesContractuelles
                   }

instance ResponseType ConsulterDonneesTechniquesContractuellesResponseType where
  configResp = ConfigResponse{
                     xmlTag = "ns7:consulterDonneesTechniquesContractuellesResponse"
                   , elementResponse = elementConsulterDonneesTechniquesContractuellesResponse
                   }


initType_ :: Bool -> String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initType_ prod myPointId autorisationClient = do
    (loginUtilisateur, _) <- getLoginContrat prod

    let requestType = ConsulterDonneesTechniquesContractuellesType{
          consulterDonneesTechniquesContractuellesType_pointId = PointIdType $ XsdString myPointId
        , consulterDonneesTechniquesContractuellesType_loginUtilisateur =  Ds.AdresseEmailType $ XsdString loginUtilisateur
        , consulterDonneesTechniquesContractuellesType_autorisationClient = Just $ Ds.BooleenType autorisationClient
        }
    return requestType

initType :: String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initType = initType_ True

initTypeTest :: String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initTypeTest = initType_ False


myrequest :: Maybe String -> IO()
myrequest mPointId = do
    env <- getEnv
    let testEnv = test env
    let myPointId = fromMaybe (T.unpack $ pointId testEnv) mPointId
    myType <- initType myPointId False
    rep  <- wsRequest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
    pPrint rep
