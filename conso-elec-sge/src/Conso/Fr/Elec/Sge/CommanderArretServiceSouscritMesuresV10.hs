{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10
Description : Webservice CommandeArretServiceSouscritMesures v1.0 — OBSOLÈTE depuis SGE v26.1

@deprecated@

Ce webservice a été supprimé en SGE v26.1 (Enedis.SGE.GUI.0474).
Utiliser 'CommanderArretServicesAccesDonneesV10' à la place.

Conservé pour compatibilité avec les environnements antérieurs à v26.1.
-}
module Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type
    ( CommanderArretServiceSouscritMesuresType(..),
      elementToXMLCommanderArretServiceSouscritMesures,
      CommanderArretServiceSouscritMesuresResponseType,
      elementCommanderArretServiceSouscritMesuresResponse,
      ArretServiceSouscritType(ArretServiceSouscritType,
                               arretServiceSouscritType_serviceSouscritId),
      DemandeType(DemandeType, demandeType_arretServiceSouscrit,
                  demandeType_donneesGenerales),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contratId, donneesGeneralesType_refFrn,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin) )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( Chaine15Type(Chaine15Type),
      PointIdType(PointIdType),
      ContratIdType(ContratIdType),
      DemandeObjetCodeType(DemandeObjetCodeType),
      AdresseEmailType(AdresseEmailType) )
    
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
 

instance RequestType CommanderArretServiceSouscritMesuresType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeArretServiceSouscritMesures/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLCommanderArretServiceSouscritMesures
                   }

instance ResponseType CommanderArretServiceSouscritMesuresResponseType where
  configResp = ConfigResponse{
                     xmlTag = "commanderArretServiceSouscritMesuresResponse"
                   , elementResponse = elementCommanderArretServiceSouscritMesuresResponse
                   }
             

initType_ :: Bool -> String -> String -> IO CommanderArretServiceSouscritMesuresType
initType_ prod myPointId serviceSouscritId = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = CommanderArretServiceSouscritMesuresType{ 
          commanderArretServiceSouscritMesuresType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refFrn = Nothing
            , donneesGeneralesType_objetCode = Ds.DemandeObjetCodeType $ Xsd.XsdString "ASS"
            , donneesGeneralesType_pointId = Ds.PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin =  Ds.AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = Ds.ContratIdType $ Xsd.XsdString contratId
            }
          , demandeType_arretServiceSouscrit = ArretServiceSouscritType
            { arretServiceSouscritType_serviceSouscritId  = Ds.Chaine15Type $ Xsd.XsdString serviceSouscritId
            }
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String  -- ^ myPointId : point de référence sur lequel on souhaite obtenir des informations.
         -> String  -- ^ serviceSouscritId : Identifiant du service souscrit de mesures à arrêter.
         -> IO CommanderArretServiceSouscritMesuresType
initType = initType_ True

initTypeTest :: String -> String -> IO CommanderArretServiceSouscritMesuresType
initTypeTest = initType_ False


myrequest :: String -> IO()
myrequest serviceSouscritId = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) serviceSouscritId 
    rep <- wsRequest myType :: IO ( Either (String, String) CommanderArretServiceSouscritMesuresResponseType )
    pPrint rep
