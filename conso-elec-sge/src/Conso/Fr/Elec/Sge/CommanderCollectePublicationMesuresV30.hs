{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30 where

import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import           Text.XML.HaXml.OneOfN ( OneOf2(OneOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type
    ( PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contratId, donneesGeneralesType_refExterne,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin),
      DemandeType(DemandeType, demandeType_accesMesures,
                  demandeType_donneesGenerales),
      DemandeAccesMesures(DemandeAccesMesures,
                          demandeAccesMesures_periodiciteTransmission,
                          demandeAccesMesures_dateDebut, demandeAccesMesures_dateFin,
                          demandeAccesMesures_declarationAccordClient,
                          demandeAccesMesures_mesuresTypeCode, demandeAccesMesures_soutirage,
                          demandeAccesMesures_injection, demandeAccesMesures_mesuresPas,
                          demandeAccesMesures_mesuresCorrigees,
                          demandeAccesMesures_transmissionRecurrente),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_choice1,
                                  declarationAccordClientType_accord),
      CommanderCollectePublicationMesuresType(..),
      CommanderCollectePublicationMesuresResponseType,
      elementToXMLCommanderCollectePublicationMesures,
      elementCommanderCollectePublicationMesuresResponse )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType(PointIdType),
      BooleenType(BooleenType),
      AdresseEmailType(AdresseEmailType),
      PeriodiciteCodeType(PeriodiciteCodeType),
      MesureTypeCodeType(MesureTypeCodeType),
      DemandeObjetCodeType(DemandeObjetCodeType),
      DateType(DateType),
      ContratIdType(ContratIdType),
      Chaine255Type(Chaine255Type) )
    
import Conso.Fr.Elec.Sge.Sge
    ( RequestType,
      ResponseType,
      Env(test, sge),
      Sge(contractId, userB2b),
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(nomClientFinalOuDenominationSociale, pointId),
      getEnv,
      sgeRequest )


instance RequestType CommanderCollectePublicationMesuresType
instance ResponseType CommanderCollectePublicationMesuresResponseType
               

initType :: Sge -> String -> Bool -> String -> String -> IO CommanderCollectePublicationMesuresType
initType envSge myPointId autorisationClient nom mesuresTypeCode = do
    let loginUtilisateur = userB2b envSge
    let contratId = contractId envSge

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
            , donneesGeneralesType_initiateurLogin =  Ds.AdresseEmailType $ Xsd.XsdString $ T.unpack loginUtilisateur
            , donneesGeneralesType_contratId = Ds.ContratIdType $ Xsd.XsdString $ T.unpack contratId
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


wsRequest :: Sge -> CommanderCollectePublicationMesuresType -> IO ()
wsRequest envSge r = sgeRequest envSge r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommandeCollectePublicationMesures/v3.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLCommanderCollectePublicationMesures
                , xmlTag = "ns4:commanderCollectePublicationMesuresResponse"
                , elementResponse = elementCommanderCollectePublicationMesuresResponse
}  



myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    let envSge = sge env
    myType <- initType envSge (T.unpack $ pointId testEnv) True 
                       (T.unpack $ nomClientFinalOuDenominationSociale testEnv) "CDC"
    wsRequest envSge myType
