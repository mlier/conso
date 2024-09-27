{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10Type
    ( Demande(Demande, demande_cadreAcces, demande_format,
              demande_pointIds, demande_dateDebut, demande_dateFin,
              demande_sens),
      PointIds(PointIds, pointIds_pointId),
      CadreAcces(CadreAcces_ACCORD_CLIENT),
      Sens(Sens_SOUTIRAGE),
      DateFin(DateFin),
      DateDebut(DateDebut),
      PointId(PointId),
      Format(Format_JSON),
      DonneesGenerales(DonneesGenerales,
                       donneesGenerales_referenceRegroupement,
                       donneesGenerales_initiateurLogin, donneesGenerales_contratId,
                       donneesGenerales_referenceDemandeur,
                       donneesGenerales_affaireReference),
      ContratId(ContratId),
      InitiateurLogin(InitiateurLogin),
      DemandePublicationMesuresFacturantes(..),
      AffaireId,
      elementAffaireId,
      elementToXMLDemandePublicationMesuresFacturantes )
    
import Conso.Fr.Elec.Sge.Sge
    ( ResponseType,
      RequestType,
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(pointId),
      Sge(contractId, userB2b),
      Env(test, sge),
      getEnv,
      sgeRequest )


instance RequestType DemandePublicationMesuresFacturantes
instance ResponseType AffaireId
               

initType :: Sge -> String -> String -> String -> IO DemandePublicationMesuresFacturantes
initType envSge myPointId dateDebut dateFin = do
    let loginUtilisateur = userB2b envSge
    let contratId = contractId envSge

    let requestType = DemandePublicationMesuresFacturantes{
          demandePublicationMesuresFacturantes_donneesGenerales = DonneesGenerales
          { donneesGenerales_initiateurLogin =  InitiateurLogin $ Xsd.XsdString $ T.unpack loginUtilisateur
          , donneesGenerales_contratId = ContratId $ Xsd.XsdString $ T.unpack contratId
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


wsRequest :: Sge -> DemandePublicationMesuresFacturantes -> IO ()
wsRequest envSge r = sgeRequest envSge r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommandeHistoriqueDonneesMesuresFacturantes/v1.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLDemandePublicationMesuresFacturantes
                , xmlTag = "v1:affaireId"
                , elementResponse = elementAffaireId
}  


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    let envSge = sge env
    myType <- initType envSge (T.unpack $ pointId testEnv) "2024-08-01" "2024-09-01"
    wsRequest envSge myType
