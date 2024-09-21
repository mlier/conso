{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    
import Conso.Fr.Elec.Sge.Sge
 


instance RequestType CommanderArretServiceSouscritMesuresType
instance ResponseType CommanderArretServiceSouscritMesuresResponseType
               

initType :: String -> String -> IO CommanderArretServiceSouscritMesuresType
initType myPointId serviceSouscritId = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv
    let contratId = contractId sgeEnv

    let requestType = CommanderArretServiceSouscritMesuresType{ 
          commanderArretServiceSouscritMesuresType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refFrn = Nothing
            , donneesGeneralesType_objetCode = Ds.DemandeObjetCodeType $ Xsd.XsdString "ASS"
            , donneesGeneralesType_pointId = Ds.PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin =  Ds.AdresseEmailType $ Xsd.XsdString $ T.unpack loginUtilisateur
            , donneesGeneralesType_contratId = Ds.ContratIdType $ Xsd.XsdString $ T.unpack contratId
            }
          , demandeType_arretServiceSouscrit = ArretServiceSouscritType
            { arretServiceSouscritType_serviceSouscritId  = Ds.Chaine15Type $ Xsd.XsdString serviceSouscritId
            }
          }
        }
    return requestType


wsRequest :: CommanderArretServiceSouscritMesuresType -> IO ()
wsRequest r = sgeRequest r configWS
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
    myType <- initType (T.unpack $ pointId testEnv) serviceSouscritId 
    wsRequest myType
