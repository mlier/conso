{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10 where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type
    ( elementAffaireId,
      elementToXMLDemandePublicationITC,
      AffaireId,
      CadreAcces(CadreAcces_ACCORD_CLIENT),
      ContratId(ContratId),
      Demande(Demande, demande_cadreAcces, demande_format,
              demande_pointIds, demande_sens),
      DemandePublicationITC(..),
      DonneesGenerales(DonneesGenerales,
                       donneesGenerales_referenceRegroupement,
                       donneesGenerales_initiateurLogin, donneesGenerales_contratId,
                       donneesGenerales_referenceDemandeur,
                       donneesGenerales_affaireReference),
      Format(Format_JSON),
      InitiateurLogin(InitiateurLogin),
      PointId(PointId),
      PointIds(PointIds, pointIds_pointId),
      Sens(Sens_SOUTIRAGE) )
  
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


instance RequestType DemandePublicationITC
instance ResponseType AffaireId
               

initType :: String -> IO DemandePublicationITC
initType myPointId = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv
    let contratId = contractId sgeEnv

    let requestType = DemandePublicationITC{
          demandePublicationITC_donneesGenerales = DonneesGenerales
          { donneesGenerales_initiateurLogin =  InitiateurLogin $ Xsd.XsdString $ T.unpack loginUtilisateur
          , donneesGenerales_contratId = ContratId $ Xsd.XsdString $ T.unpack contratId
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


wsRequest :: DemandePublicationITC -> IO ()
wsRequest r = sgeRequest r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommandeInformationsTechniquesEtContractuelles/v1.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLDemandePublicationITC
                , xmlTag = "v1:affaireId"
                , elementResponse = elementAffaireId
}  



myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
    wsRequest myType
