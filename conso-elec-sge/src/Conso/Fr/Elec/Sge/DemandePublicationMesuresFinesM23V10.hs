{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

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
      Env(test),
      getEnv,
      sgeRequest,
      getLoginContrat )


instance RequestType DemandePublicationMesuresFines
instance ResponseType AffaireId
               

initType :: Bool -> String -> IO DemandePublicationMesuresFines
initType prod myPointId = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = DemandePublicationMesuresFines{
          demandePublicationMesuresFines_donneesGenerales = DonneesGenerales
          { donneesGenerales_initiateurLogin =  InitiateurLogin $ Xsd.XsdString loginUtilisateur
          , donneesGenerales_contratId = ContratId $ Xsd.XsdString contratId
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



wsRequest :: Bool -> DemandePublicationMesuresFines -> IO ( Either (String, String) AffaireId )
wsRequest prod r = sgeRequest prod r configWS
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
    myType <- initType True (T.unpack $ pointId testEnv)
    rep <- wsRequest True myType
    pPrint rep
