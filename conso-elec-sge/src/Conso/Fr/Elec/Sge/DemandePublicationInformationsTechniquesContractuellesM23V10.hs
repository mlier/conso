{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type
    ( DemandePublicationITC(..),
      elementToXMLDemandePublicationITC,
      AffaireId,
      PointId(PointId),
      Format(Format_JSON),
      elementAffaireId,
      CadreAcces(CadreAcces_ACCORD_CLIENT),
      ContratId(ContratId),
      Demande(Demande, demande_cadreAcces, demande_format,
              demande_pointIds, demande_sens),
      DonneesGenerales(DonneesGenerales,
                       donneesGenerales_referenceRegroupement,
                       donneesGenerales_initiateurLogin, donneesGenerales_contratId,
                       donneesGenerales_referenceDemandeur,
                       donneesGenerales_affaireReference),
      InitiateurLogin(InitiateurLogin),
      PointIds(PointIds, pointIds_pointId),
      Sens(Sens_SOUTIRAGE) )
  
import Conso.Fr.Elec.Sge.Sge
    ( RequestType(..),
      ResponseType(..),
      ConfigRequest(ConfigRequest, elementToXMLRequest, urlSge,
                    soapAction),
      ConfigResponse(ConfigResponse, elementResponse, xmlTag),
      getEnv,
      getLoginContrat,
      wsRequest,
      Env(test),
      Test(pointId) )


instance RequestType DemandePublicationITC where
  configReq = ConfigRequest{
                     urlSge = "/CommandeInformationsTechniquesEtContractuelles/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLDemandePublicationITC
                   }

instance ResponseType AffaireId where
  configResp = ConfigResponse{
                     xmlTag = "v1:affaireId"
                   , elementResponse = elementAffaireId
                   }
               

initType_ :: Bool -> String -> IO DemandePublicationITC
initType_ prod myPointId = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = DemandePublicationITC{
          demandePublicationITC_donneesGenerales = DonneesGenerales
          { donneesGenerales_initiateurLogin =  InitiateurLogin $ Xsd.XsdString loginUtilisateur
          , donneesGenerales_contratId = ContratId $ Xsd.XsdString contratId
          , donneesGenerales_referenceDemandeur = Nothing
          , donneesGenerales_affaireReference = Nothing
          , donneesGenerales_referenceRegroupement = Nothing
          }
        , demandePublicationITC_demande = Demande
          { demande_format = Just Format_JSON
          , demande_pointIds = PointIds
            { pointIds_pointId = [PointId $ Xsd.XsdString myPointId]
            }
          , demande_sens = Sens_SOUTIRAGE
          , demande_cadreAcces = CadreAcces_ACCORD_CLIENT
          }
        }
    return requestType

initType :: String -> IO DemandePublicationITC
initType = initType_ True

initTypeTest :: String -> IO DemandePublicationITC
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    rep <- wsRequest myType :: IO ( Either (String, String) AffaireId )
    pPrint rep
