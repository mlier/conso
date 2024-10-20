{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10 where

import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2) ) 
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( CommanderAccesDonneesMesuresType(..),
      elementToXMLCommanderAccesDonneesMesures,
      CommanderAccesDonneesMesuresResponseType,
      DemandeObjetCodeType(DemandeObjetCodeType),
      PointIdType(PointIdType),
      AdresseEmailType(AdresseEmailType),
      ContratIdType(ContratIdType),
      BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      TypeDonneesType(TypeDonneesType),
      DateType(DateType),
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom),
      elementCommanderAccesDonneesMesuresResponse,
      AccesDonneesType(AccesDonneesType, accesDonneesType_injection,
                       accesDonneesType_dateDebut, accesDonneesType_dateFin,
                       accesDonneesType_declarationAccordClient,
                       accesDonneesType_typeDonnees, accesDonneesType_soutirage),
      ContratType(ContratType, contratType_contratType,
                  contratType_contratId, contratType_acteurMarcheCode),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_choice1,
                                  declarationAccordClientType_accord),
      DemandeType(DemandeType, demandeType_accesDonnees,
                  demandeType_donneesGenerales),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contrat, donneesGeneralesType_refExterne,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin) )
    
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
 

instance RequestType CommanderAccesDonneesMesuresType where
  configReq = ConfigRequest{
                     urlSge = "/CommanderAccesDonneesMesures/v1.0"
                   , soapAction = " "
                   , elementToXMLRequest = elementToXMLCommanderAccesDonneesMesures
                   }

instance ResponseType CommanderAccesDonneesMesuresResponseType where
  configResp = ConfigResponse{
                    xmlTag = "ns4:commanderAccesDonneesMesuresResponse"
                   , elementResponse = elementCommanderAccesDonneesMesuresResponse
                   }
              

initType_ :: Bool -> String -> Bool -> String -> String -> IO CommanderAccesDonneesMesuresType
initType_ prod myPointId autorisationClient nom typeDonnees = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    currentTime <- getCurrentTime
    let dateDebut = formatTime defaultTimeLocale "%Y-%m-%d" currentTime

    let troisans = addDays (3*364) (utctDay currentTime)
    let dateFin = formatTime defaultTimeLocale "%Y-%m-%d" troisans

    let requestType = CommanderAccesDonneesMesuresType{
          commanderAccesDonneesMesuresType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refExterne = Nothing
            , donneesGeneralesType_objetCode = DemandeObjetCodeType $ Xsd.XsdString "AME"
            , donneesGeneralesType_pointId = PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin =  AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contrat = ContratType
              { contratType_contratId = Just $ ContratIdType $ Xsd.XsdString contratId
              , contratType_acteurMarcheCode = Nothing
              , contratType_contratType = Nothing
              }
            }
          , demandeType_accesDonnees = AccesDonneesType
            { accesDonneesType_dateDebut = DateType $ Xsd.Date dateDebut
            , accesDonneesType_dateFin = Just $ DateType $ Xsd.Date dateFin
            , accesDonneesType_declarationAccordClient = DeclarationAccordClientType
              { declarationAccordClientType_accord = BooleenType autorisationClient
              , declarationAccordClientType_choice1 = Just $ OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString nom
                  , personnePhysiqueType_prenom = Nothing
                  } 
              }
            , accesDonneesType_typeDonnees = TypeDonneesType $ Xsd.XsdString typeDonnees
            , accesDonneesType_soutirage = Just $ BooleenType True
            , accesDonneesType_injection = Just $ BooleenType False
            }
          }
        }
    return requestType

initType :: String -> Bool -> String -> String -> IO CommanderAccesDonneesMesuresType
initType = initType_ True

initTypeTest :: String -> Bool -> String -> String -> IO CommanderAccesDonneesMesuresType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) True 
                        (T.unpack $ nomClientFinalOuDenominationSociale testEnv) "CDC" 
    rep <- wsRequest myType :: IO ( Either (String, String) CommanderAccesDonneesMesuresResponseType )
    pPrint rep
