{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-|
Module      : Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10
Description : Webservice B2B ConsultationDonneesTechniquesContractuelles v1.0 (Enedis.SGE.GUI.0464 v1.4.0)

Permet de consulter les données techniques et contractuelles d'un point de livraison
(PRM) : puissance souscrite, tarif, segment, adresse, etc.

Disponible pour tous les segments C1–C5 et P1–P4.
L'accord du client (@autorisationClient@) est requis pour les Tiers.
-}
module Conso.Fr.Elec.Sge.ConsulterDonneesTechniquesContractuellesV10 (
   initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

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
      wsRequest, xmlRequest, wsRequestTest, xmlRequestTest,
      SgeEnv(test),
      Test(pointId) )


instance RequestType ConsulterDonneesTechniquesContractuellesType where
  configReq = ConfigRequest{
                     urlSge = "/ConsultationDonneesTechniquesContractuelles/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLConsulterDonneesTechniquesContractuelles
                   }

instance ResponseType ConsulterDonneesTechniquesContractuellesResponseType where
  configResp = ConfigResponse{
                     xmlTag = "consulterDonneesTechniquesContractuellesResponse"
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

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String    -- ^ myPointId : point de référence sur lequel on souhaite obtenir des informations.
         -> Bool      -- ^ autorisationClient : autorisation donnée par le gestionnaire du contrat d'électricité.
         -> IO ConsulterDonneesTechniquesContractuellesType
initType = initType_ True

-- | Comme 'initType' mais sur le serveur d'homologation.
initTypeTest :: String -> Bool -> IO ConsulterDonneesTechniquesContractuellesType
initTypeTest = initType_ False


-- | Exemple d'appel en production avec le PRM de test configuré dans le fichier YAML.
myrequest :: Maybe String -> IO()
myrequest mPointId = do
    env <- getEnv
    let testEnv = test env
    let myPointId = fromMaybe (T.unpack $ pointId testEnv) mPointId
    myType <- initType myPointId False
    rep  <- wsRequest myType :: IO (Either (String, String) ConsulterDonneesTechniquesContractuellesResponseType)
    pPrint rep
