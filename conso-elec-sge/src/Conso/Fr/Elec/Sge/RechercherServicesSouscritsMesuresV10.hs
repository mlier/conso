{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10 where

import           Data.Text ( Text )
import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), Boolean )
import           Text.Pretty.Simple (pPrint)

import           Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
import           Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type
import           Conso.Fr.Elec.Sge.Sge
    

instance RequestType RechercherServicesSouscritsMesuresType
instance ResponseType RechercherServicesSouscritsMesuresResponseType


initType :: String -> IO RechercherServicesSouscritsMesuresType
initType pointId = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv
    let contratId = contractId sgeEnv

    let requestType = RechercherServicesSouscritsMesuresType{ 
              rechercherServicesSouscritsMesuresType_criteres = CriteresType
                { criteresType_pointId = PointIdType $ XsdString pointId
                , criteresType_contratId = ContratIdType $ XsdString $ T.unpack contratId
                }
            , rechercherServicesSouscritsMesuresType_loginUtilisateur = Ds.AdresseEmailType $ XsdString $ T.unpack loginUtilisateur
            }
    return requestType


wsRequest :: RechercherServicesSouscritsMesuresType -> IO ()
wsRequest r = sgeRequest r configWS
    where configWS = ConfigWS{
                          urlSge = "/RechercheServicesSouscritsMesures/v1.0"
                        , soapAction = "nimportequoimaispasvide"
                        , elementToXMLRequest = elementToXMLRechercherServicesSouscritsMesures
                        , xmlTag = "ns4:rechercherServicesSouscritsMesuresResponse" 
                        , elementResponse = elementRechercherServicesSouscritsMesuresResponse
    }

myrequest :: IO()
myrequest = do 
    myType <- initType "21429667044956"
    wsRequest myType
    pPrint myType
