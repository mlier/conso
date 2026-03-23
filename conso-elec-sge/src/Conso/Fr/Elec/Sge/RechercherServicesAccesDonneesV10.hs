{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10
Description : Webservice B2B RechercheServicesAccesDonnees v1.0 (Enedis.SGE.GUI.0537 v1.0.2)

Permet de lister les services d'accès aux données (SAD) actifs sur un PRM.
Les identifiants retournés sont utilisés par 'CommanderArretServicesAccesDonneesV10'
et 'CommanderRenouvellementServicesAccesDonneesV10'.
-}
module Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

import qualified Data.Text as T
import Text.XML.HaXml.Schema.PrimitiveTypes (XsdString(XsdString))
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
    ( elementRechercherServicesAccesDonneesReponse,
      elementToXMLRechercherServicesAccesDonnees,
      CriteresType(CriteresType, criteresType_pointId, criteresType_contratId),
      RechercherServicesAccesDonneesReponseType,
      RechercherServicesAccesDonneesType(..) )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( AdresseEmailType(AdresseEmailType),
      ContratIdType(ContratIdType),
      PointIdType(PointIdType) )

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


instance RequestType RechercherServicesAccesDonneesType where
  configReq = ConfigRequest{
                     urlSge = "/RechercheServicesAccesDonnees/v1.0"
                   , soapAction = " "
                   , elementToXMLRequest = elementToXMLRechercherServicesAccesDonnees
                   }

instance ResponseType RechercherServicesAccesDonneesReponseType where
  configResp = ConfigResponse{
                    xmlTag = "rechercherServicesAccesDonneesReponse"
                   , elementResponse = elementRechercherServicesAccesDonneesReponse
                   }


initType_ :: Bool -> String -> IO RechercherServicesAccesDonneesType
initType_ prod myPointId = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = RechercherServicesAccesDonneesType
          { rechercherServicesAccesDonneesType_criteres =
            [ CriteresType
              { criteresType_pointId = [Ds.PointIdType $ XsdString myPointId]
              , criteresType_contratId = [Ds.ContratIdType $ XsdString contratId]
              }
            ]
          , rechercherServicesAccesDonneesType_loginUtilisateur =
            [ Ds.AdresseEmailType $ XsdString loginUtilisateur ]
          }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String  -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> IO RechercherServicesAccesDonneesType
initType = initType_ True

-- | Comme 'initType' mais sur le serveur d'homologation.
initTypeTest :: String -> IO RechercherServicesAccesDonneesType
initTypeTest = initType_ False


-- | Exemple d'appel en production avec le PRM de test configuré dans le fichier YAML.
myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    rep <- wsRequest myType :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
    pPrint rep
