{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30 where

import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import           Text.XML.HaXml.OneOfN ( OneOf2(OneOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type
    ( CommanderCollectePublicationMesuresType(..),
      elementToXMLCommanderCollectePublicationMesures,
      CommanderCollectePublicationMesuresResponseType,
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom),
      elementCommanderCollectePublicationMesuresResponse,
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_choice1,
                                  declarationAccordClientType_accord),
      DemandeAccesMesures(DemandeAccesMesures,
                          demandeAccesMesures_periodiciteTransmission,
                          demandeAccesMesures_dateDebut, demandeAccesMesures_dateFin,
                          demandeAccesMesures_declarationAccordClient,
                          demandeAccesMesures_mesuresTypeCode, demandeAccesMesures_soutirage,
                          demandeAccesMesures_injection, demandeAccesMesures_mesuresPas,
                          demandeAccesMesures_mesuresCorrigees,
                          demandeAccesMesures_transmissionRecurrente),
      DemandeType(DemandeType, demandeType_accesMesures,
                  demandeType_donneesGenerales),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contratId, donneesGeneralesType_refExterne,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin) )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      PointIdType(PointIdType),
      DateType(DateType),
      ContratIdType(ContratIdType),
      DemandeObjetCodeType(DemandeObjetCodeType),
      MesureTypeCodeType(MesureTypeCodeType),
      PeriodiciteCodeType(PeriodiciteCodeType),
      AdresseEmailType(AdresseEmailType) )
    
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
      Test(nomClientFinalOuDenominationSociale, pointId) )


instance RequestType CommanderCollectePublicationMesuresType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeCollectePublicationMesures/v3.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLCommanderCollectePublicationMesures
                   }

instance ResponseType CommanderCollectePublicationMesuresResponseType where
  configResp = ConfigResponse{
                     xmlTag = "ns4:commanderCollectePublicationMesuresResponse"
                   , elementResponse = elementCommanderCollectePublicationMesuresResponse
                   }
              

initType_ :: Bool -> String -> Bool -> String -> String -> IO CommanderCollectePublicationMesuresType
initType_ prod myPointId autorisationClient nom mesuresTypeCode = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    currentTime <- getCurrentTime
    let dateDebut = formatTime defaultTimeLocale "%Y-%m-%d" currentTime

    let troisans = addDays (3*365) (utctDay currentTime)
    let dateFin = formatTime defaultTimeLocale "%Y-%m-%d" troisans

    let requestType = CommanderCollectePublicationMesuresType{
          commanderCollectePublicationMesuresType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refExterne = Nothing
            , donneesGeneralesType_objetCode = Ds.DemandeObjetCodeType $ Xsd.XsdString "AME"
            , donneesGeneralesType_pointId = Ds.PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin =  Ds.AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = Ds.ContratIdType $ Xsd.XsdString contratId
            }
          , demandeType_accesMesures = DemandeAccesMesures
            { demandeAccesMesures_dateDebut = Ds.DateType $ Xsd.Date dateDebut
            , demandeAccesMesures_dateFin = Just $ Ds.DateType $ Xsd.Date dateFin
            , demandeAccesMesures_declarationAccordClient = DeclarationAccordClientType
              { declarationAccordClientType_accord = Ds.BooleenType autorisationClient
              , declarationAccordClientType_choice1 = OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Ds.Chaine255Type $ Xsd.XsdString nom
                  , personnePhysiqueType_prenom = Nothing
                  } 
              }
            , demandeAccesMesures_mesuresTypeCode = Ds.MesureTypeCodeType $ Xsd.XsdString mesuresTypeCode
            , demandeAccesMesures_soutirage = Ds.BooleenType True
            , demandeAccesMesures_injection = Ds.BooleenType False
            , demandeAccesMesures_mesuresPas = Nothing
            , demandeAccesMesures_mesuresCorrigees = Just $ Ds.BooleenType False
            , demandeAccesMesures_transmissionRecurrente = Ds.BooleenType True
            , demandeAccesMesures_periodiciteTransmission = Just $ Ds.PeriodiciteCodeType $ Xsd.XsdString "P1D"
            }
          }
        }

    return requestType

initType :: String -> Bool -> String -> String -> IO CommanderCollectePublicationMesuresType
initType = initType_ True

initTypeTest :: String -> Bool -> String -> String -> IO CommanderCollectePublicationMesuresType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) True 
                       (T.unpack $ nomClientFinalOuDenominationSociale testEnv) "CDC"
    rep <- wsRequest myType :: IO ( Either (String, String) CommanderCollectePublicationMesuresResponseType )
    pPrint rep
