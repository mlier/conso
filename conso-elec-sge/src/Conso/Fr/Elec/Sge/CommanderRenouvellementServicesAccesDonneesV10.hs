{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, AccordPersonneType(..), Sens(..)
) where

import qualified Data.Text as T
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2, TwoOf2) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
    ( AdresseEmailType(AdresseEmailType),
      BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      ContratIdType(ContratIdType),
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


initType_ :: Bool -> String -> Sens -> AccordPersonneType -> [String] -> IO RenouvelerServicesAccesType
initType_ prod myPointId sens accordPersonneType serviceIds = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

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
            , donneesGeneralesType_dateFin = Nothing
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
         -> IO RenouvelerServicesAccesType
initType = initType_ True

initTypeTest :: String -> Sens -> AccordPersonneType -> [String] -> IO RenouvelerServicesAccesType
initTypeTest = initType_ False


myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
                       SensSOUTIRAGE
                       (AccordPersonnePhysiqueNom (T.unpack $ nomClientFinalOuDenominationSociale testEnv))
                       []
    rep <- wsRequest myType :: IO (Either (String, String) RenouvelerServicesAccesResponseType)
    pPrint rep

data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)

data AccordPersonneType
    = AccordPersonnePhysiqueNom String
    | AccordPersonneMoraleDenominationSociale String
    deriving (Eq,Show)
