{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10
Description : Webservice B2B CommandeServicesAccesDonnees v1.0 (Enedis.SGE.GUI.0531 v1.1.0)

Permet de souscrire un service d'accès aux données de mesures (SAD) sur un PRM.
Un SAD actif autorise la consultation des données via 'ConsulterMesuresDetailleesV3'
avec @cadreAcces = SERVICE_ACCES@.

@typeDonnees@ accepte : @CDC@ (courbe), @IDX@ (index), @PMAX@ (Pmax), @ENERGIE@.
@duree = Nothing@ crée un SAD ouvert (SAD-NR2 → SGT509 si durée > 3 ans).
-}
module Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest,
  AccordPersonneType(..), Sens(..), Periodicite(..), periodiciteStr
) where

import qualified Data.Text as T
import           Data.Time ( getZonedTime, zonedTimeToLocalTime, localDay, addDays, formatTime, defaultTimeLocale )
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2, TwoOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
    ( AdresseEmailType(AdresseEmailType),
      BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      CommanderServicesAccesDonneesResponseType,
      DateType(DateType),
      CommanderServicesAccesDonneesType(..),
      ContratIdType(ContratIdType),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_accord,
                                  declarationAccordClientType_choice1),
      DemandeType(DemandeType, demandeType_donneesGenerales,
                  demandeType_servicesSouscrits),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_refExterne,
                           donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin,
                           donneesGeneralesType_contratId,
                           donneesGeneralesType_dateFin,
                           donneesGeneralesType_sens,
                           donneesGeneralesType_declarationAccordClient),
      PersonneMoraleType(PersonneMoraleType,
                         personneMoraleType_denominationSociale),
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_civilite,
                           personnePhysiqueType_nom,
                           personnePhysiqueType_prenom),
      PointIdType(PointIdType),
      SensType(SensTypeSOUTIRAGE, SensTypeINJECTION),
      ServicesSouscritsType(ServicesSouscritsType),
      OptionPublicationType(OptionPublicationType,
                            optionPublicationType_mesuresCorrigees,
                            optionPublicationType_periodiciteTransmission),
      OptionsPublicationType(OptionsPublicationType),
      PeriodiciteTransmissionType(PeriodiciteTransmissionType),
      ServiceSouscritType(ServiceSouscritType,
                          serviceSouscritType_typeDonnees,
                          serviceSouscritType_optionsPublication),
      TypeDonneesType(TypeDonneesType),
      elementCommanderServicesAccesDonneesResponse,
      elementToXMLCommanderServicesAccesDonnees )

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


instance RequestType CommanderServicesAccesDonneesType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeServicesAccesDonnees/v1.0"
                   , soapAction = " "
                   , elementToXMLRequest = elementToXMLCommanderServicesAccesDonnees
                   }

instance ResponseType CommanderServicesAccesDonneesResponseType where
  configResp = ConfigResponse{
                    xmlTag = "commanderServicesAccesDonneesResponse"
                   , elementResponse = elementCommanderServicesAccesDonneesResponse
                   }


initType_ :: Bool -> String -> Sens -> Maybe AccordPersonneType -> String -> Maybe Integer -> Maybe Periodicite -> IO CommanderServicesAccesDonneesType
initType_ prod myPointId sens accordPersonneType typeDonnees duree mPeriodicite = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    zonedTime <- getZonedTime
    let today = localDay (zonedTimeToLocalTime zonedTime)
    let dateFin = case duree of
            Just d  -> Just $ DateType $ Xsd.Date $ formatTime defaultTimeLocale "%Y-%m-%d"
                         $ addDays d today
            Nothing -> Nothing

    let sensType = case sens of
            SensSOUTIRAGE -> SensTypeSOUTIRAGE
            SensINJECTION -> SensTypeINJECTION

    let accordDecl = case accordPersonneType of
            Nothing -> Just $ DeclarationAccordClientType
              { declarationAccordClientType_accord = BooleenType False
              , declarationAccordClientType_choice1 = Nothing
              }
            Just ap -> Just $ DeclarationAccordClientType
              { declarationAccordClientType_accord = BooleenType True
              , declarationAccordClientType_choice1 = case ap of
                  AccordPersonnePhysiqueNom nom -> Just $ OneOf2 $ PersonnePhysiqueType
                    { personnePhysiqueType_civilite = Nothing
                    , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString nom
                    , personnePhysiqueType_prenom = Nothing
                    }
                  AccordPersonneMoraleDenominationSociale nom -> Just $ TwoOf2 $ PersonneMoraleType
                    { personneMoraleType_denominationSociale = Chaine255Type $ Xsd.XsdString nom
                    }
              }

    let optsPub = fmap (\p -> OptionsPublicationType
          [ OptionPublicationType
              { optionPublicationType_mesuresCorrigees        =
                  if typeDonnees == "CDC" then Just (BooleenType False) else Nothing
              , optionPublicationType_periodiciteTransmission =
                  PeriodiciteTransmissionType (Xsd.XsdString (periodiciteStr p))
              }
          ]) mPeriodicite

    let requestType = CommanderServicesAccesDonneesType{
          commanderServicesAccesDonneesType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_refExterne = Nothing
            , donneesGeneralesType_pointId = PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin = AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = ContratIdType $ Xsd.XsdString contratId
            , donneesGeneralesType_dateFin = dateFin
            , donneesGeneralesType_sens = sensType
            , donneesGeneralesType_declarationAccordClient = accordDecl
            }
          , demandeType_servicesSouscrits = ServicesSouscritsType
            [ ServiceSouscritType
              { serviceSouscritType_typeDonnees        = TypeDonneesType $ Xsd.XsdString typeDonnees
              , serviceSouscritType_optionsPublication = optsPub
              }
            ]
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String                   -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Sens                     -- ^ sens : indique le sens de l'énergie.
         -> Maybe AccordPersonneType -- ^ accordPersonneType : certifie l'accord du client.
                                     --
                                     -- - Just PersonnePhysique : accord True, nom de la personne,
                                     -- - Just PersonneMorale : accord True, dénomination morale,
                                     -- - Nothing : accord False (SAD-NR1 → SGT566).
         -> String                   -- ^ typeDonnees : type de données demandé (CDC, IDX, PMAX, ENERGIE).
         -> Maybe Integer            -- ^ duree : durée en jours depuis aujourd'hui, ou Nothing (SAD-NR2 → SGT509 si > 3 ans).
         -> Maybe Periodicite        -- ^ periodicite : périodicité de publication (@P1D@ = quotidien, @P7D@ = hebdomadaire, @P1M@ = mensuel). 'Nothing' = accès consultation uniquement, sans dépôt SFTP automatique.
         -> IO CommanderServicesAccesDonneesType
initType = initType_ True

-- | Comme 'initType' mais sur le serveur d'homologation.
initTypeTest :: String -> Sens -> Maybe AccordPersonneType -> String -> Maybe Integer -> Maybe Periodicite -> IO CommanderServicesAccesDonneesType
initTypeTest = initType_ False


-- | Exemple d'appel en production avec le PRM de test configuré dans le fichier YAML.
myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
                       SensSOUTIRAGE
                       (Just $ AccordPersonnePhysiqueNom (T.unpack $ nomClientFinalOuDenominationSociale testEnv))
                       "CDC" Nothing (Just P1D)
    rep <- wsRequest myType :: IO (Either (String, String) CommanderServicesAccesDonneesResponseType)
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

-- | Périodicité de publication automatique sur SFTP (format ISO 8601).
data Periodicite
    = P1D  -- ^ Quotidien
    | P7D  -- ^ Hebdomadaire
    | P1M  -- ^ Mensuel
    deriving (Eq, Show, Enum, Bounded)

periodiciteStr :: Periodicite -> String
periodiciteStr P1D = "P1D"
periodiciteStr P7D = "P7D"
periodiciteStr P1M = "P1M"
