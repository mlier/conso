{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10Type
    ( DemandePublicationMesuresFacturantes(..),
      elementToXMLDemandePublicationMesuresFacturantes,
      AffaireId,
      PointId(PointId),
      Format(Format_JSON),
      elementAffaireId,
      CadreAcces(CadreAcces_ACCORD_CLIENT),
      ContratId(ContratId),
      DateDebut(DateDebut),
      DateFin(DateFin),
      Demande(Demande, demande_cadreAcces, demande_format,
              demande_pointIds, demande_dateDebut, demande_dateFin,
              demande_sens),
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
   

instance RequestType DemandePublicationMesuresFacturantes where
  configReq = ConfigRequest{
                     urlSge = "/CommandeHistoriqueDonneesMesuresFacturantes/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLDemandePublicationMesuresFacturantes
                   }

instance ResponseType AffaireId where
  configResp = ConfigResponse{
                     xmlTag = "v1:affaireId"
                   , elementResponse = elementAffaireId
                   }
              

initType_ :: Bool -> String -> String -> String -> IO DemandePublicationMesuresFacturantes
initType_ prod myPointId dateDebut dateFin = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = DemandePublicationMesuresFacturantes{
          demandePublicationMesuresFacturantes_donneesGenerales = DonneesGenerales
          { donneesGenerales_initiateurLogin =  InitiateurLogin $ Xsd.XsdString loginUtilisateur
          , donneesGenerales_contratId = ContratId $ Xsd.XsdString contratId
          , donneesGenerales_referenceDemandeur = Nothing
          , donneesGenerales_affaireReference = Nothing
          , donneesGenerales_referenceRegroupement = Nothing
          }
        , demandePublicationMesuresFacturantes_demande = Demande
          { demande_format= Just Format_JSON
          , demande_pointIds = PointIds
            { pointIds_pointId = [PointId $ Xsd.XsdString myPointId]
            }
          , demande_dateDebut = DateDebut $ Xsd.Date dateDebut
          , demande_dateFin = DateFin $ Xsd.Date dateFin
          , demande_sens = Sens_SOUTIRAGE
          , demande_cadreAcces = CadreAcces_ACCORD_CLIENT
          }
        }
    return requestType

initType :: String -> String -> String -> IO DemandePublicationMesuresFacturantes
initType = initType_ True

initTypeTest :: String -> String -> String -> IO DemandePublicationMesuresFacturantes
initTypeTest = initType_ False 


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) "2024-08-01" "2024-09-01"
    rep <- wsRequest myType :: IO ( Either (String, String) AffaireId )
    pPrint rep
