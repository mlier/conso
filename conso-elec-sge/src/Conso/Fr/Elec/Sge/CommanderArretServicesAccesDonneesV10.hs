{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, Sens(..)
) where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( AdresseEmailType(AdresseEmailType),
      Chaine15Type(Chaine15Type),
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


instance RequestType CommanderArretServicesAccesDonneesType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeArretServicesAccesDonnees/v1.0"
                   , soapAction = " "
                   , elementToXMLRequest = elementToXMLCommanderArretServicesAccesDonnees
                   }

instance ResponseType CommanderArretServicesAccesDonneesResponseType where
  configResp = ConfigResponse{
                    xmlTag = "commanderArretServicesAccesDonneesResponse"
                   , elementResponse = elementCommanderArretServicesAccesDonneesResponse
                   }


initType_ :: Bool -> String -> Sens -> [String] -> IO CommanderArretServicesAccesDonneesType
initType_ prod myPointId sens serviceIds = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let sensType = case sens of
            SensSOUTIRAGE -> SensTypeSOUTIRAGE
            SensINJECTION -> SensTypeINJECTION

    let requestType = CommanderArretServicesAccesDonneesType{
          commanderArretServicesAccesDonneesType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refExterne = Nothing
            , donneesGeneralesType_pointId = Ds.PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin = Ds.AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = Ds.ContratIdType $ Xsd.XsdString contratId
            , donneesGeneralesType_sens = sensType
            }
          , demandeType_servicesSouscrits = ServicesSouscritsType
            { servicesSouscritsType_serviceSouscritId = map (Ds.Chaine15Type . Xsd.XsdString) serviceIds
            }
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String    -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Sens      -- ^ sens : indique le sens de l'énergie.
         -> [String]  -- ^ serviceIds : liste des identifiants de services à arrêter.
         -> IO CommanderArretServicesAccesDonneesType
initType = initType_ True

initTypeTest :: String -> Sens -> [String] -> IO CommanderArretServicesAccesDonneesType
initTypeTest = initType_ False


myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
                       SensSOUTIRAGE
                       []
    rep <- wsRequest myType :: IO (Either (String, String) CommanderArretServicesAccesDonneesResponseType)
    pPrint rep

data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)
