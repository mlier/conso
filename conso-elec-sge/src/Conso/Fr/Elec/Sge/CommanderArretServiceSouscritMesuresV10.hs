{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type
    ( elementCommanderArretServiceSouscritMesuresResponse,
      elementToXMLCommanderArretServiceSouscritMesures,
      ArretServiceSouscritType(ArretServiceSouscritType,
                               arretServiceSouscritType_serviceSouscritId),
      CommanderArretServiceSouscritMesuresResponseType,
      CommanderArretServiceSouscritMesuresType(..),
      DemandeType(DemandeType, demandeType_arretServiceSouscrit,
                  demandeType_donneesGenerales),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contratId, donneesGeneralesType_refFrn,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin) )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( AdresseEmailType(AdresseEmailType),
      Chaine15Type(Chaine15Type),
      ContratIdType(ContratIdType),
      DemandeObjetCodeType(DemandeObjetCodeType),
      PointIdType(PointIdType) )
    
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
 


instance RequestType CommanderArretServiceSouscritMesuresType
instance ResponseType CommanderArretServiceSouscritMesuresResponseType
               

initType :: Bool -> String -> String -> IO CommanderArretServiceSouscritMesuresType
initType prod myPointId serviceSouscritId = do
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


wsRequest :: Bool -> CommanderArretServiceSouscritMesuresType -> 
              IO ( Either (String, String) CommanderArretServiceSouscritMesuresResponseType )
wsRequest prod r = sgeRequest prod r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommandeArretServiceSouscritMesures/v1.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLCommanderArretServiceSouscritMesures
                , xmlTag = "ns4:commanderArretServiceSouscritMesuresResponse"
                , elementResponse = elementCommanderArretServiceSouscritMesuresResponse
}  


myrequest :: String -> IO()
myrequest serviceSouscritId = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType True (T.unpack $ pointId testEnv) serviceSouscritId 
    rep <- wsRequest True myType
    pPrint rep
