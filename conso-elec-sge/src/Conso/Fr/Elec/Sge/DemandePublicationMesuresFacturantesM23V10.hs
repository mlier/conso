{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10
Description : Webservice M023 CommandeHistoriqueDonneesMesuresFacturantes v1.0 (Enedis.SGE.GUI.0502 v1.4.0)

Demande asynchrone de publication des mesures facturantes (énergie active par
classe temporelle et calendrier) pour un ou plusieurs PRM.
Les données sont publiées via le flux M023 R67.

La réponse est un identifiant d'affaire (@AffaireId@) ; les données sont
publiées ultérieurement dans les fichiers M023.
-}
module Conso.Fr.Elec.Sge.DemandePublicationMesuresFacturantesM23V10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

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

   

instance RequestType DemandePublicationMesuresFacturantes where
  configReq = ConfigRequest{
                     urlSge = "/CommandeHistoriqueDonneesMesuresFacturantes/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLDemandePublicationMesuresFacturantes
                   }

instance ResponseType AffaireId where
  configResp = ConfigResponse{
                     xmlTag = "affaireId"
                   , elementResponse = elementAffaireId
                   }
              

initType_ :: Bool -> [String] -> String -> String -> Sens -> CadreAcces -> IO DemandePublicationMesuresFacturantes
initType_ prod myPointsId dateDebut dateFin sens cadreAcces = do
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
            { pointIds_pointId = map (PointId . Xsd.XsdString) myPointsId
            }
          , demande_dateDebut = DateDebut $ Xsd.Date dateDebut
          , demande_dateFin = DateFin $ Xsd.Date dateFin
          , demande_sens = sens 
          , demande_cadreAcces = cadreAcces
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE (flux R67).
initType :: [String]    -- ^ myPointsId : liste des identifiants PRM des points sur lesquels porte la demande.
         -> String      -- ^ dateDebut : date de début souhaitée pour la consultation des mesures (date incluse).
         -> String      -- ^ dateFin : date de fin souhaitée pour la consultation des mesures (date exclue).
         -> Sens        -- ^ sens : indique le Sens de l’énergie circulant vers le réseau d’Enedis : 
                        -- 
                        -- - INJECTION,
                        -- - SOUTIRAGE.
         -> CadreAcces  -- ^ cadreAcces : indique à quel titre l’acteur consulte les données de mesures :
                        --
                        -- - ACCORD_CLIENT si le demandeur accède aux données de mesures au titre d’un accord du client,
                        -- - SERVICE_ACCES si le demandeur a souscrit au préalable à un service d’accès aux données de mesures,
                        -- - EST_TITULAIRE si le demandeur est le fournisseur titulaire du contrat unique sur l’ensemble 
                        --   des PRM de la demande et sur la période.
         -> IO DemandePublicationMesuresFacturantes
initType = initType_ True

-- | Comme 'initType' mais sur le serveur d'homologation.
initTypeTest :: [String] -> String -> String -> Sens -> CadreAcces -> IO DemandePublicationMesuresFacturantes
initTypeTest = initType_ False


-- | Exemple d'appel en production avec le PRM de test configuré dans le fichier YAML.
myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType [T.unpack $ pointId testEnv] "2024-08-01" "2024-09-01" Sens_SOUTIRAGE CadreAcces_ACCORD_CLIENT
    rep <- wsRequest myType :: IO ( Either (String, String) AffaireId )
    pPrint rep
