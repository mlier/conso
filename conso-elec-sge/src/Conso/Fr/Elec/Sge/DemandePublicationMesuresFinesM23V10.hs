{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
    ( DemandePublicationMesuresFines(..),
      elementToXMLDemandePublicationMesuresFines,
      AffaireId,
      PointId(PointId),
      Format(FormatJSON),
      elementAffaireId,
      CadreAcces(CadreAccesACCORDCLIENT),
      ContratId(ContratId),
      DateDebut(DateDebut),
      DateFin(DateFin),
      Demande(Demande, demande_cadreAcces, demande_format,
              demande_pointIds, demande_mesuresTypeCode,
              demande_mesuresCorrigees, demande_dateDebut, demande_dateFin,
              demande_sens),
      DonneesGenerales(DonneesGenerales,
                       donneesGenerales_referenceRegroupement,
                       donneesGenerales_initiateurLogin, donneesGenerales_contratId,
                       donneesGenerales_referenceDemandeur,
                       donneesGenerales_affaireReference),
      InitiateurLogin(InitiateurLogin),
      MesuresTypeCode(MesuresTypeCodeINDEX),
      PointIds(PointIds, pointIds_pointId),
      Sens(SensSOUTIRAGE) )

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
  

instance RequestType DemandePublicationMesuresFines where
  configReq = ConfigRequest{
                     urlSge = "/CommandeHistoriqueDonneesMesuresFines/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLDemandePublicationMesuresFines
                   }

instance ResponseType AffaireId where
  configResp = ConfigResponse{
                     xmlTag = "v1:affaireId"
                   , elementResponse = elementAffaireId
                   }
            

initType_ :: Bool -> String -> IO DemandePublicationMesuresFines
initType_ prod myPointId = do
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

initType :: String -> IO DemandePublicationMesuresFines
initType = initType_ True

initTypeTest :: String -> IO DemandePublicationMesuresFines
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    rep <- wsRequest myType :: IO ( Either (String, String) AffaireId )
    pPrint rep
