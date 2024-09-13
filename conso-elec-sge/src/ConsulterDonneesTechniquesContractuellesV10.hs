{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ConsulterDonneesTechniquesContractuellesV10 where

import           Data.Text ( Text )
import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), Boolean )
import           Text.Pretty.Simple (pPrint)

import           EnedisDictionnaireTypeSimpleV50 as Ds
import           ConsulterDonneesTechniquesContractuellesV10Type
import Sge
    ( ResponseType,
      RequestType(..),
      Sge(..),
      Env(sge),
      getEnv,
      sgeRequest )


instance RequestType ConsulterDonneesTechniquesContractuellesType where
    config _ = ( "https://sge-b2b.enedis.fr/ConsultationDonneesTechniquesContractuelles/v1.0"
               , "nimportequoimaispasvide"
               , elementToXMLConsulterDonneesTechniquesContractuelles 
               , "ns7:consulterDonneesTechniquesContractuellesResponse" )

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

sgeRequestFinal :: ConsulterDonneesTechniquesContractuellesType -> IO ()
sgeRequestFinal r = sgeRequest r elementConsulterDonneesTechniquesContractuellesResponse

myrequest :: IO()
myrequest = do 
    myType <- initType "21429667044956" False
    sgeRequestFinal myType
