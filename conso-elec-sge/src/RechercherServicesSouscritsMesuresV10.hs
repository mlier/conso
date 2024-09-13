{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RechercherServicesSouscritsMesuresV10 where

import           Data.Text ( Text )
import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString), Boolean )
import           Text.Pretty.Simple (pPrint)

import           EnedisDictionnaireTypeSimpleV50 as Ds
import           RechercherServicesSouscritsMesuresV10Type
import Sge
    ( ResponseType,
      RequestType(..),
      Sge(..),
      Env(sge),
      getEnv,
      sgeRequest )


instance RequestType RechercherServicesSouscritsMesuresType where
    config _ = ( "https://sge-b2b.enedis.fr/RechercheServicesSouscritsMesures/v1.0"
               , "nimportequoimaispasvide"
               , elementToXMLRechercherServicesSouscritsMesures
               , "ns4:rechercherServicesSouscritsMesuresResponse" )

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

sgeRequestFinal :: RechercherServicesSouscritsMesuresType -> IO ()
sgeRequestFinal r = sgeRequest r elementRechercherServicesSouscritsMesuresResponse

myrequest :: IO()
myrequest = do 
    myType <- initType "21429667044956"
    sgeRequestFinal myType
    pPrint myType
