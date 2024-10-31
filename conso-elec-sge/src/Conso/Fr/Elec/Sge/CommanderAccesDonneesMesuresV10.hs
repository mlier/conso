{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, AccordPersonneType, Sens
) where

import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2, TwoOf2) ) 
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
    ( elementCommanderAccesDonneesMesuresResponse,
      elementToXMLCommanderAccesDonneesMesures,
      AccesDonneesType(AccesDonneesType, accesDonneesType_injection,
                       accesDonneesType_dateDebut, accesDonneesType_dateFin,
                       accesDonneesType_declarationAccordClient,
                       accesDonneesType_typeDonnees, accesDonneesType_soutirage),
      AdresseEmailType(AdresseEmailType),
      BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      CommanderAccesDonneesMesuresResponseType,
      CommanderAccesDonneesMesuresType(..),
      ContratIdType(ContratIdType),
      ContratType(ContratType, contratType_contratType,
                  contratType_contratId, contratType_acteurMarcheCode),
      DateType(DateType),
      DeclarationAccordClientType(DeclarationAccordClientType,
                                  declarationAccordClientType_choice1,
                                  declarationAccordClientType_accord),
      DemandeObjetCodeType(DemandeObjetCodeType),
      DemandeType(DemandeType, demandeType_accesDonnees,
                  demandeType_donneesGenerales),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_contrat, donneesGeneralesType_refExterne,
                           donneesGeneralesType_objetCode, donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin),
      PersonneMoraleType(PersonneMoraleType,
                         personneMoraleType_denominationSociale),
      PersonnePhysiqueType(PersonnePhysiqueType,
                           personnePhysiqueType_prenom, personnePhysiqueType_civilite,
                           personnePhysiqueType_nom),
      PointIdType(PointIdType),
      TypeDonneesType(TypeDonneesType) )

    
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
              

initType_ :: Bool -> String -> Maybe Integer -> AccordPersonneType -> String -> Sens -> IO CommanderAccesDonneesMesuresType
initType_ prod myPointId duree accordPersonneType typeDonnees sens = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    currentTime <- getCurrentTime
    let dateDebut = formatTime defaultTimeLocale "%Y-%m-%d" currentTime

    let dateFin = case duree of
            Just d -> Just $ DateType $ Xsd.Date $ formatTime defaultTimeLocale "%Y-%m-%d" $ addDays d (utctDay currentTime) 
            Nothing -> Nothing
    
    let soutirage = case sens of 
            SensSOUTIRAGE -> True
            SensINJECTION -> False

    let personTypeChoice = case accordPersonneType of 
            AccordPersonnePhysiqueNom nom -> Just $ OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString nom
                  , personnePhysiqueType_prenom = Nothing
                  } 
            AccordPersonneMoraleDenominationSociale nom -> Just $ TwoOf2 $ PersonneMoraleType
                  { personneMoraleType_denominationSociale = Chaine255Type $ Xsd.XsdString nom
                  } 

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
            , accesDonneesType_dateFin = dateFin
            , accesDonneesType_declarationAccordClient = DeclarationAccordClientType
              { declarationAccordClientType_accord = BooleenType True
              , declarationAccordClientType_choice1 = personTypeChoice
              }
            , accesDonneesType_typeDonnees = TypeDonneesType $ Xsd.XsdString typeDonnees
            , accesDonneesType_soutirage = Just $ BooleenType soutirage
            , accesDonneesType_injection = Just $ BooleenType (not soutirage)
            }
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String              -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Maybe Integer       -- ^ duree : durée de la demande au service : 
                                --
                                -- - Pour un point C5 et P4, la durée ne peut excéder 3 ans,
                                -- - Pour un point C1-C4 et P1-P3, si une durée est fournie, elle ne peut 
                                --   excéder 3 ans.
         -> AccordPersonneType  -- ^ accordPersonneType : certifie l'accord du client et son type : 
                                --
                                -- - PersonnePhysique donne le nom de la personne physique qui a donné accord,
                                -- - PersonneMorale donne la dénomination morale qui a donné son accord.
         -> String              -- ^ typeDonnees : pour un point C5, les valeurs possibles sont : 
                                -- 
                                -- - CDC courbe de mesure,
                                -- - IDX index quotidien,
                                -- - PMAX puissance maximale,
                                -- - ENERGIE énergie globale.
                                --
                                -- Pour un point C1-C4, P1-P3 et P4 les valeurs possibles sont :
                                --
                                -- - CDC courbe de mesure,
                                -- - IDX index quotidien,
                                -- - ENERGIE énergie globale.
         -> Sens                -- ^ sens : indique le Sens de l’énergie circulant vers le réseau d’Enedis : 
                                -- 
                                -- - INJECTION,
                                -- - SOUTIRAGE.
         -> IO CommanderAccesDonneesMesuresType
initType = initType_ True

initTypeTest :: String -> Maybe Integer -> AccordPersonneType -> String -> Sens -> IO CommanderAccesDonneesMesuresType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) (Just $ 3*364) 
                        ( AccordPersonnePhysiqueNom (T.unpack $ nomClientFinalOuDenominationSociale testEnv) )
                        "CDC" SensSOUTIRAGE
    rep <- wsRequest myType :: IO ( Either (String, String) CommanderAccesDonneesMesuresResponseType )
    pPrint rep

data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)

data AccordPersonneType
    = AccordPersonnePhysiqueNom String
    | AccordPersonneMoraleDenominationSociale String
    deriving (Eq,Show)