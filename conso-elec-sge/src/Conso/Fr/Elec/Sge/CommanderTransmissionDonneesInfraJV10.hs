{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10 where

import qualified Data.Text as T
import           Text.XML.HaXml.OneOfN ( OneOf2(OneOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Type
    ( PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contratId, donneesGeneralesType_refExterne,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin),
      DemandeType(DemandeType, demandeType_accesDonnees,
                  demandeType_donneesGenerales),
      DemandeAccesDonneesType(DemandeAccesDonneesType,
                              demandeAccesDonneesType_ptd,
                              demandeAccesDonneesType_declarationAccordClient,
                              demandeAccesDonneesType_injection,
                              demandeAccesDonneesType_soutirage, demandeAccesDonneesType_cdc,
                              demandeAccesDonneesType_idx),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_choice3,
                                  declarationAccordClientType_accordClient,
                                  declarationAccordClientType_injection,
                                  declarationAccordClientType_soutirage),
      CommanderTransmissionDonneesInfraJType(..),
      CommanderTransmissionDonneesInfraJResponseType,
      elementToXMLCommanderTransmissionDonneesInfraJ,
      elementCommanderTransmissionDonneesInfraJResponse )

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType(PointIdType),
      DemandeObjetCodeType(DemandeObjetCodeType),
      ContratIdType(ContratIdType),
      Chaine255Type(Chaine255Type),
      BooleenType(BooleenType),
      AdresseEmailType(AdresseEmailType) )
    
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


instance RequestType CommanderTransmissionDonneesInfraJType
instance ResponseType CommanderTransmissionDonneesInfraJResponseType
               

initType :: Bool -> String -> Bool -> String  -> IO CommanderTransmissionDonneesInfraJType
initType prod myPointId autorisationClient nom = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let requestType = CommanderTransmissionDonneesInfraJType{
          commanderTransmissionDonneesInfraJType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refExterne = Nothing
            , donneesGeneralesType_objetCode = Ds.DemandeObjetCodeType $ Xsd.XsdString "AME"
            , donneesGeneralesType_pointId = Ds.PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin =  Ds.AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = Ds.ContratIdType $ Xsd.XsdString contratId
            }
          , demandeType_accesDonnees = DemandeAccesDonneesType
            { demandeAccesDonneesType_declarationAccordClient = [DeclarationAccordClientType
              { declarationAccordClientType_accordClient = Ds.BooleenType autorisationClient
              , declarationAccordClientType_injection = Ds.BooleenType False
              , declarationAccordClientType_soutirage = Ds.BooleenType True
              , declarationAccordClientType_choice3 = OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Ds.Chaine255Type $ Xsd.XsdString nom
                  , personnePhysiqueType_prenom = Nothing
                  } 
              }]
            , demandeAccesDonneesType_injection = Ds.BooleenType False
            , demandeAccesDonneesType_soutirage = Ds.BooleenType True
            , demandeAccesDonneesType_cdc  = Ds.BooleenType True
            , demandeAccesDonneesType_idx = Ds.BooleenType False
            , demandeAccesDonneesType_ptd = Ds.BooleenType False
            }
          }
        }
    return requestType


wsRequest :: Bool -> CommanderTransmissionDonneesInfraJType -> 
              IO ( Either (String, String) CommanderTransmissionDonneesInfraJResponseType )
wsRequest prod r = sgeRequest prod r configWS
    where configWS = ConfigWS{
                  urlSge = "/CommandeTransmissionDonneesInfraJ/v1.0"
                , soapAction = "nimportequoimaispasvide"
                , elementToXMLRequest = elementToXMLCommanderTransmissionDonneesInfraJ
                , xmlTag = "ns4:commanderTransmissionDonneesInfraJResponse"
                , elementResponse = elementCommanderTransmissionDonneesInfraJResponse
}  



myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType True (T.unpack $ pointId testEnv) True 
                       (T.unpack $ nomClientFinalOuDenominationSociale testEnv)
    rep <- wsRequest True myType
    pPrint rep 
