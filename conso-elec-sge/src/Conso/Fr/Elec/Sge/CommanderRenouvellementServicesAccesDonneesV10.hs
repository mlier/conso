{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10
Description : Webservice B2B CommandeRenouvellementServicesAccesDonnees v1.0 (Enedis.SGE.GUI.0531 v1.1.0)

Permet de renouveler un ou plusieurs services d'accès aux données (SAD) existants
sur un PRM, en prolongeant leur durée à partir de la date courante.
Les identifiants de services (@serviceIds@) sont obtenus via
'RechercherServicesAccesDonneesV10'.
-}
module Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, AccordPersonneType(..), Sens(..)
) where

import qualified Data.Text as T
import           Data.Time ( getCurrentTime, addDays, formatTime, defaultTimeLocale, utctDay )
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2, TwoOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
    ( AdresseEmailType(AdresseEmailType),
      BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      ContratIdType(ContratIdType),
      DateType(DateType),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_accord,
                                  declarationAccordClientType_choice1),
      DemandeType(DemandeType, demandeType_donneesGenerales,
                  demandeType_servicesSouscrits),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin,
                           donneesGeneralesType_contratId,
                           donneesGeneralesType_sens,
                           donneesGeneralesType_declarationAccordClient,
                           donneesGeneralesType_dateFin),
      PersonneMoraleType(PersonneMoraleType,
                         personneMoraleType_denominationSociale),
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_civilite,
                           personnePhysiqueType_nom,
                           personnePhysiqueType_prenom),
      PointIdType(PointIdType),
      RenouvelerServicesAccesResponseType,
      RenouvelerServicesAccesType(..),
      SensType(SensTypeSOUTIRAGE, SensTypeINJECTION),
      ServiceIdType(ServiceIdType),
      ServicesSouscritsType(ServicesSouscritsType,
                            servicesSouscritsType_serviceSouscritId),
      elementCommanderRenouvellementServicesAccesDonneesResponse,
      elementToXMLCommanderRenouvellementServicesAccesDonnees )

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


instance RequestType RenouvelerServicesAccesType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeRenouvellementServicesAccesDonnees/v1.0"
                   , soapAction = " "
                   , elementToXMLRequest = elementToXMLCommanderRenouvellementServicesAccesDonnees
                   }

instance ResponseType RenouvelerServicesAccesResponseType where
  configResp = ConfigResponse{
                    xmlTag = "commanderRenouvellementServicesAccesDonneesResponse"
                   , elementResponse = elementCommanderRenouvellementServicesAccesDonneesResponse
                   }


initType_ :: Bool -> String -> Sens -> AccordPersonneType -> [String] -> Maybe Integer -> IO RenouvelerServicesAccesType
initType_ prod myPointId sens accordPersonneType serviceIds duree = do
    (loginUtilisateur, contratId) <- getLoginContrat prod
    currentTime <- getCurrentTime
    let dateFin = case duree of
            Just d  -> Just $ DateType $ Xsd.Date
                         $ formatTime defaultTimeLocale "%Y-%m-%d"
                         $ addDays d (utctDay currentTime)
            Nothing -> Nothing

    let sensType = case sens of
            SensSOUTIRAGE -> SensTypeSOUTIRAGE
            SensINJECTION -> SensTypeINJECTION

    let personTypeChoice = case accordPersonneType of
            AccordPersonnePhysiqueNom nom -> Just $ OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString nom
                  , personnePhysiqueType_prenom = Nothing
                  }
            AccordPersonneMoraleDenominationSociale nom -> Just $ TwoOf2 $ PersonneMoraleType
                  { personneMoraleType_denominationSociale = Chaine255Type $ Xsd.XsdString nom
                  }

    let requestType = RenouvelerServicesAccesType{
          renouvelerServicesAccesType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_pointId = PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin = AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = ContratIdType $ Xsd.XsdString contratId
            , donneesGeneralesType_sens = sensType
            , donneesGeneralesType_declarationAccordClient = Just $ DeclarationAccordClientType
              { declarationAccordClientType_accord = BooleenType True
              , declarationAccordClientType_choice1 = personTypeChoice
              }
            , donneesGeneralesType_dateFin = dateFin
            }
          , demandeType_servicesSouscrits = ServicesSouscritsType
            { servicesSouscritsType_serviceSouscritId = map (ServiceIdType . Xsd.XsdString) serviceIds
            }
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String              -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Sens                -- ^ sens : indique le sens de l'énergie.
         -> AccordPersonneType  -- ^ accordPersonneType : certifie l'accord du client.
         -> [String]            -- ^ serviceIds : liste des identifiants de services à renouveler.
         -> Maybe Integer       -- ^ duree : durée en jours depuis aujourd'hui, ou Nothing (pas de date de fin).
         -> IO RenouvelerServicesAccesType
initType = initType_ True

-- | Comme 'initType' mais sur le serveur d'homologation.
initTypeTest :: String -> Sens -> AccordPersonneType -> [String] -> Maybe Integer -> IO RenouvelerServicesAccesType
initTypeTest = initType_ False


-- | Exemple d'appel en production avec le PRM de test configuré dans le fichier YAML.
myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
                       SensSOUTIRAGE
                       (AccordPersonnePhysiqueNom (T.unpack $ nomClientFinalOuDenominationSociale testEnv))
                       []
                       Nothing
    rep <- wsRequest myType :: IO (Either (String, String) RenouvelerServicesAccesResponseType)
    pPrint rep

-- | Sens de circulation de l'énergie par rapport au réseau Enedis.
data Sens
    = SensSOUTIRAGE  -- ^ Énergie soutirée du réseau (consommation)
    | SensINJECTION  -- ^ Énergie injectée dans le réseau (production)
    deriving (Eq,Show,Enum)

-- | Identité de la personne ayant donné son accord pour l'accès aux données.
data AccordPersonneType
    = AccordPersonnePhysiqueNom String                 -- ^ Nom de la personne physique ayant donné accord
    | AccordPersonneMoraleDenominationSociale String   -- ^ Dénomination sociale de la personne morale
    deriving (Eq,Show)
