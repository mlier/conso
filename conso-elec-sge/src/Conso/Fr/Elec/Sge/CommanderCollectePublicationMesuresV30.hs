{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, AccordPersonneType, Sens
) where

import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Text.XML.HaXml.OneOfN ( OneOf2(TwoOf2, OneOf2) ) 
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type
    ( elementCommanderCollectePublicationMesuresResponse,
      elementToXMLCommanderCollectePublicationMesures,
      CommanderCollectePublicationMesuresResponseType,
      CommanderCollectePublicationMesuresType(..),
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
                           donneesGeneralesType_initiateurLogin),
      PersonneMoraleType(PersonneMoraleType,
                         personneMoraleType_denominationSociale),
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom) )
   

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
              

initType_ :: Bool -> String -> Maybe Integer -> AccordPersonneType -> String
          -> Sens -> Bool -> Maybe Bool -> Maybe String 
          -> IO CommanderCollectePublicationMesuresType
initType_ prod myPointId duree  accordPersonneType mesuresTypeCode 
            sens transmissionRecurrente mesuresCorrigees periodiciteTransmission = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    currentTime <- getCurrentTime
    let dateDebut = Ds.DateType $ Xsd.Date $ formatTime defaultTimeLocale "%Y-%m-%d" currentTime

    let dateFin = case duree of
            Just d -> Just $ DateType $ Xsd.Date $ formatTime defaultTimeLocale "%Y-%m-%d" $ addDays d (utctDay currentTime) 
            Nothing -> Nothing

    let soutirage = case sens of 
            SensSOUTIRAGE -> True
            SensINJECTION -> False

    let personTypeChoice = case accordPersonneType of 
            AccordPersonnePhysiqueNom nom -> OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString nom
                  , personnePhysiqueType_prenom = Nothing
                  } 
            AccordPersonneMoraleDenominationSociale nom -> TwoOf2 $ PersonneMoraleType
                  { personneMoraleType_denominationSociale = Chaine255Type $ Xsd.XsdString nom
                  } 

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
            { demandeAccesMesures_dateDebut = dateDebut
            , demandeAccesMesures_dateFin = dateFin
            , demandeAccesMesures_declarationAccordClient = DeclarationAccordClientType
              { declarationAccordClientType_accord = Ds.BooleenType True
              , declarationAccordClientType_choice1 = personTypeChoice
              }
            , demandeAccesMesures_mesuresTypeCode = Ds.MesureTypeCodeType $ Xsd.XsdString mesuresTypeCode
            , demandeAccesMesures_soutirage = Ds.BooleenType soutirage
            , demandeAccesMesures_injection = Ds.BooleenType $ not soutirage
            , demandeAccesMesures_mesuresPas = Nothing
            , demandeAccesMesures_mesuresCorrigees = Ds.BooleenType <$> mesuresCorrigees --Just $ Ds.BooleenType False
            , demandeAccesMesures_transmissionRecurrente = Ds.BooleenType transmissionRecurrente -- True
            , demandeAccesMesures_periodiciteTransmission = Ds.PeriodiciteCodeType . Xsd.XsdString <$> periodiciteTransmission --Just $ Ds.PeriodiciteCodeType $ Xsd.XsdString "P1D"
            }
          }
        }

    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String              -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Maybe Integer       -- ^ duree : durée de la demande transmission récurrente de données de mesure ou de collecte 
                                --   de la courbe de charge :
                                -- - Pour un point C5 et P4, la durée ne peut excéder 3 ans et doit être supérieure 
                                --   à la date de fin du service actif dans le cas d’un renouvellement,
                                -- - Pour un point C1-C4 et P1-P3, si une durée est fournie, elle ne peut 
                                --   excéder 3 ans.
         -> AccordPersonneType  -- ^ accordPersonneType : certifie l'accord du client et son type : 
                                --
                                -- - PersonnePhysique donne le nom de la personne physique qui a donné accord,
                                -- - PersonneMorale donne la dénomination morale qui a donné son accord.   

         -> String              -- ^ mesuresTypeCode : type de mesure demandé : courbe de charge, 
                                --   index quotidiens et puissances maximales quotidiennes ou index et 
                                --   autres données du compteur : 
                                --
                                -- - CDC pour une demande de transmission récurrente de la courbe de charge ou 
                                --   de collecte de la courbe de charge,
                                -- - IDX pour une demande de transmission récurrente d’index quotidiens et de 
                                --   puissances maximales quotidiennes-(C5) ou de transmission quotidienne des 
                                --   index et autres données du compteur (C1-C4 et P1-P3).
                                --
                                --   Pour le segment P4, seule la demande de collecte de la courbe de charge étant 
                                --   possible, seule la valeur CDC est autorisée.
         -> Sens                -- ^ sens : indique le Sens de l’énergie circulant vers le réseau d’Enedis : 
                                -- 
                                -- - INJECTION,
                                -- - SOUTIRAGE.
         -> Bool                -- ^ transmissionRecurrente : indique si la demande consiste en une transmission 
                                --   récurrente de données de mesure ou une collecte de la courbe de charge : 
                                --
                                -- - Pour la courbe de charge (mesuresTypeCode = CDC) :
                                --
                                --     - True en cas de demande de transmission récurrente,
                                --     - False en cas de demande de collecte de la courbe de charge.
                                --
                                -- - Pour les index et Pmax quotidiens ou index et autres données du compteur 
                                --   (mesuresTypeCode = IDX) :
                                --     - True pour le P4, la transmission des données n’étant pas encore disponible, 
                                --       seule la valeur False est autorisée.
         -> Maybe Bool          -- ^ mesuresCorrigees : dans le cas de demandes de transmission de la courbe de 
                                --   charge C1-C4, booléen permettant d’indiquer si la courbe demandée est brute ou corrigée. 
                                --   Obligatoire si la transmission de courbe de charge est demandée (balises mesuresTypeCode 
                                --   = CDC et transmissionRecurrente = true) :
                                --
                                -- - Pour le C1-C4 et P1-P3 :
                                --
                                --     - True pour une courbe de charge corrigée
                                --     - False pour une courbe de charge brute.
                                --
                                -- - Pour le C5 et P4 : la courbe de charge corrigée n’est pas disponible. Cette balise 
                                --   doit être renseignée à False. Ignorée pour une demande de collecte de la courbe 
                                --   de charge, de transmission récurrente des index quotidiens et des puissances 
                                --   maximales quotidiennes ou des index et autres données du compteur.
                                --   
                                --   A noter que pour une fréquence de publication quotidienne de courbe de charge, 
                                --   seules les données brutes sont disponibles. La publication des données corrigées de 
                                --   la courbe de charge se fait uniquement pour les publications hebdomadaires et mensuelles.
         -> Maybe String        -- ^ periodiciteTransmission : fréquence de la transmission des données de mesure. 
                                --   Obligatoire dans le cas d’une demande de transmission récurrente de données de mesure.
                                --
                                -- - Valeurs autorisées pour le C1-C4 et P1-P3 :
                                --
                                --      - P1D (quotidienne),
                                --      - P7D (hebdomadaire)
                                --      - P1M (mensuelle)
                                --
                                -- - Valeurs autorisées pour le C5 :
                                --
                                --       - P1D (quotidienne),
                                --       - P1M (mensuelle)
                                --
                                --   En cas de transmission des index et autres données du compteur pour le C1-C4 et P1-P3, 
                                --   la seule valeur autorisée est P1D (quotidienne).
                                --   Ignorée dans le cas d’une demande de collecte de la courbe de charge.
                                --   Pour le P4, la transmission des données n’étant pas encore disponible, 
                                --   cette donnée ne doit pas être transmise.
         -> IO CommanderCollectePublicationMesuresType
initType = initType_ True  

initTypeTest :: String -> Maybe Integer -> AccordPersonneType -> String
          -> Sens -> Bool -> Maybe Bool -> Maybe String -> IO CommanderCollectePublicationMesuresType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) (Just (3*365)) 
                       ( AccordPersonnePhysiqueNom (T.unpack $ nomClientFinalOuDenominationSociale testEnv) )
                       "CDC" SensSOUTIRAGE True (Just False) (Just "P1D")
    rep <- wsRequest myType :: IO ( Either (String, String) CommanderCollectePublicationMesuresResponseType )
    pPrint rep

data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)

data AccordPersonneType
    = AccordPersonnePhysiqueNom String
    | AccordPersonneMoraleDenominationSociale String
    deriving (Eq,Show)