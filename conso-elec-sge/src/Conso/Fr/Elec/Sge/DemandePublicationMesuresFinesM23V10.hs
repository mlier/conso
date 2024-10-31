{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

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
      MesuresCorrigees,
      MesuresTypeCode(MesuresTypeCodeINDEX),
      PointId(PointId),
      PointIds(PointIds, pointIds_pointId),
      Sens(SensSOUTIRAGE) )


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
            

initType_ :: Bool -> [String] -> MesuresTypeCode -> Maybe MesuresCorrigees -> String -> String 
          -> Sens -> CadreAcces -> IO DemandePublicationMesuresFines
initType_ prod myPointsId mesuresTypeCode mesuresCorrigees dateDebut dateFin sens cadreAcces = do
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
            { pointIds_pointId = map (PointId . Xsd.XsdString) myPointsId
            }
          , demande_mesuresTypeCode = mesuresTypeCode
          , demande_mesuresCorrigees = mesuresCorrigees
          , demande_dateDebut = DateDebut $ Xsd.Date dateDebut
          , demande_dateFin = DateFin $ Xsd.Date dateFin
          , demande_sens = sens
          , demande_cadreAcces = cadreAcces
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: [String]                -- ^ myPointsId : liste des identifiants PRM des points sur lesquels porte la demande.
         -> MesuresTypeCode         -- ^ mesuresTypeCode : type de mesures demandé :
                                    --
                                    -- - COURBES pour une courbe (de puissance ou de tension) (flux R63),
                                    -- - INDEX pour les index (flux R64).
                                    -- - ENERGIE pour les énergies globales quotidiennes (flux R65),
                                    -- - PMAX pour les puissances maximales quotidiennes (flux R66),
         -> Maybe MesuresCorrigees  -- ^ mesuresCorrigees : donnée attendue uniquement dans le cas d’une demande liée à des 
                                    --   données de mesures ‘Courbe de charge’. 
                                    --
                                    -- - Pour les C5/P4, la balise doit être renseignée à « false »
                                    -- - Pour les C1-C4/P1-P3, la balise doit être renseignée à :
                                    --
                                    --     - « true » si l’on souhaite recevoir les données corrigées,
                                    --     - « false » si l’on souhaite recevoir les données brutes.
         -> String                  -- ^ dateDebut : date de début souhaitée pour la consultation des mesures (date incluse).
         -> String                  -- ^ dateFin : date de fin souhaitée pour la consultation des mesures (date exclue).
         -> Sens                    -- ^ sens : indique le Sens de l’énergie circulant vers le réseau d’Enedis : 
                                    -- 
                                    -- - INJECTION,
                                    -- - SOUTIRAGE.
         -> CadreAcces              -- ^ cadreAcces : indique à quel titre l’acteur consulte les données de mesures :
                                    --
                                    -- - ACCORD_CLIENT si le demandeur accède aux données de mesures au titre d’un accord du client,
                                    -- - SERVICE_ACCES si le demandeur a souscrit au préalable à un service d’accès aux données 
                                    --   de mesures.
                                    
         -> IO DemandePublicationMesuresFines
initType = initType_ True

initTypeTest :: [String] -> MesuresTypeCode -> Maybe MesuresCorrigees -> String -> String 
          -> Sens -> CadreAcces -> IO DemandePublicationMesuresFines
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType [T.unpack $ pointId testEnv] MesuresTypeCodeINDEX Nothing "2024-08-01" 
                        "2024-09-01" SensSOUTIRAGE CadreAccesACCORDCLIENT
    rep <- wsRequest myType :: IO ( Either (String, String) AffaireId )
    pPrint rep
