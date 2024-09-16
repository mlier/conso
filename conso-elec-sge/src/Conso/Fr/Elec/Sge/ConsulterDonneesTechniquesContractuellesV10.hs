{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10 where

import           Data.Text ( Text )
import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), )
import Text.XML.HaXml.Schema.Schema as Schema ( Content, XMLParser )
import           Text.Pretty.Simple (pPrint)

import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
import           Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10Type
import           Conso.Fr.Elec.Sge.Sge


instance RequestType ConsulterDonneesTechniquesContractuellesType
instance ResponseType ConsulterDonneesTechniquesContractuellesResponseType
               

initType :: String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initType pointId autorisationClient = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv

    let requestType = ConsulterDonneesTechniquesContractuellesType{ 
          consulterDonneesTechniquesContractuellesType_pointId = PointIdType $ XsdString pointId
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



myrequest :: IO()
myrequest = do 
    myType <- initType "21429667044956" False
    wsRequest myType
