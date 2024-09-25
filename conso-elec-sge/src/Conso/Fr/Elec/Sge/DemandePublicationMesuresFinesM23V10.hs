{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
    ( elementAffaireId,
      elementToXMLDemandePublicationMesuresFines,
      AffaireId,
      CadreAcces(CadreAccesACCORDCLIENT),
      ContratId(ContratId),
      DateDebut(DateDebut),
      DateFin(DateFin),
      Demande(Demande, demande_cadreAcces, demande_format,
              demande_pointIds, demande_mesuresTypeCode,
              demande_mesuresCorrigees, demande_dateDebut, demande_dateFin,
              demande_sens),
      DemandePublicationMesuresFines(..),
      DonneesGenerales(DonneesGenerales,
                       donneesGenerales_referenceRegroupement,
                       donneesGenerales_initiateurLogin, donneesGenerales_contratId,
                       donneesGenerales_referenceDemandeur,
                       donneesGenerales_affaireReference),
      Format(FormatJSON),
      InitiateurLogin(InitiateurLogin),
      MesuresTypeCode(MesuresTypeCodeINDEX),
      PointId(PointId),
      PointIds(PointIds, pointIds_pointId),
      Sens(SensSOUTIRAGE) )
  

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


instance RequestType DemandePublicationMesuresFines
instance ResponseType AffaireId
               

initType :: String -> IO DemandePublicationMesuresFines
initType myPointId = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv
    let contratId = contractId sgeEnv

    let requestType = DemandePublicationMesuresFines{
          demandePublicationMesuresFines_donneesGenerales = DonneesGenerales
          { donneesGenerales_initiateurLogin =  InitiateurLogin $ Xsd.XsdString $ T.unpack loginUtilisateur
          , donneesGenerales_contratId = ContratId $ Xsd.XsdString $ T.unpack contratId
          , donneesGenerales_referenceDemandeur = Nothing
          , donneesGenerales_affaireReference = Nothing
          , donneesGenerales_referenceRegroupement = Nothing
          }
        , demandePublicationMesuresFines_demande = Demande
          { demande_format = Just FormatJSON
          , demande_pointIds = PointIds
            { pointIds_pointId = [PointId $ Xsd.XsdString myPointId]
            }
          , demande_mesuresTypeCode = MesuresTypeCodeINDEX
          , demande_mesuresCorrigees = Nothing
          , demande_dateDebut = DateDebut $ Xsd.Date "2024-08-01"
          , demande_dateFin = DateFin $ Xsd.Date "2024-09-01"
          , demande_sens = SensSOUTIRAGE
          , demande_cadreAcces = CadreAccesACCORDCLIENT
          }
        }
    return requestType



wsRequest :: DemandePublicationMesuresFines -> IO ()
wsRequest r = sgeRequest r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommandeHistoriqueDonneesMesuresFines/v1.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLDemandePublicationMesuresFines
                , xmlTag = "v1:affaireId"
                , elementResponse = elementAffaireId
}  


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    wsRequest myType
