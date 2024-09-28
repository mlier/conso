{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10 where

import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2) ) 

import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( CommanderAccesDonneesMesuresType(..),
      CommanderAccesDonneesMesuresResponseType,
      DemandeType(DemandeType, demandeType_accesDonnees,
                  demandeType_donneesGenerales),
      Chaine255Type(Chaine255Type),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contrat, donneesGeneralesType_refExterne,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin),
      AccesDonneesType(AccesDonneesType, accesDonneesType_injection,
                       accesDonneesType_dateDebut, accesDonneesType_dateFin,
                       accesDonneesType_declarationAccordClient,
                       accesDonneesType_typeDonnees, accesDonneesType_soutirage),
      DemandeObjetCodeType(DemandeObjetCodeType),
      PointIdType(PointIdType),
      ContratType(ContratType, contratType_contratType,
                  contratType_contratId, contratType_acteurMarcheCode),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_choice1,
                                  declarationAccordClientType_accord),
      BooleenType(BooleenType),
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom),
      DateType(DateType),
      TypeDonneesType(TypeDonneesType),
      ContratIdType(ContratIdType),
      AdresseEmailType(AdresseEmailType),
      elementToXMLCommanderAccesDonneesMesures,
      elementCommanderAccesDonneesMesuresResponse )
    
import Conso.Fr.Elec.Sge.Sge
    ( ResponseType,
      RequestType,
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(nomClientFinalOuDenominationSociale, pointId),
      Env(test),
      getEnv,
      sgeRequest,
      getLoginContrat )
 


instance RequestType CommanderAccesDonneesMesuresType
instance ResponseType CommanderAccesDonneesMesuresResponseType
               

initType :: Bool -> String -> Bool -> String -> String -> IO CommanderAccesDonneesMesuresType
initType prod myPointId autorisationClient nom typeDonnees = do
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


wsRequest :: Bool -> CommanderAccesDonneesMesuresType -> IO ()
wsRequest prod r = sgeRequest prod r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommanderAccesDonneesMesures/v1.0"
                , soapAction = " "
                , elementToXMLRequest = elementToXMLCommanderAccesDonneesMesures
                , xmlTag = "ns4:commanderAccesDonneesMesuresResponse"
                , elementResponse = elementCommanderAccesDonneesMesuresResponse
}  


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType True (T.unpack $ pointId testEnv) True 
                        (T.unpack $ nomClientFinalOuDenominationSociale testEnv) "CDC" 
    wsRequest True myType
