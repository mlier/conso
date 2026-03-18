{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

initTypeTest :: String -> IO RechercherServicesAccesDonneesType
initTypeTest = initType_ False


myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    rep <- wsRequest myType :: IO (Either (String, String) RechercherServicesAccesDonneesReponseType)
    pPrint rep
