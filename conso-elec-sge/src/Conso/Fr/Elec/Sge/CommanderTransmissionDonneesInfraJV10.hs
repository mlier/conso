{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, AccordPersonneType(..), Sens(..)
) where

import qualified Data.Text as T
import Text.XML.HaXml.OneOfN ( OneOf2(OneOf2, TwoOf2) ) 
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Type


import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( BooleenType(BooleenType),
      Chaine255Type(Chaine255Type),
      PointIdType(PointIdType),
      ContratIdType(ContratIdType),
      DemandeObjetCodeType(DemandeObjetCodeType),
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


instance RequestType CommanderTransmissionDonneesInfraJType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeTransmissionDonneesInfraJ/v1.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLCommanderTransmissionDonneesInfraJ
                   }

instance ResponseType CommanderTransmissionDonneesInfraJResponseType where
  configResp = ConfigResponse{
                     xmlTag = "commanderTransmissionDonneesInfraJResponse"
                   , elementResponse = elementCommanderTransmissionDonneesInfraJResponse
                   }
              

initType_ :: Bool -> String -> Maybe AccordPersonneType -> Sens -> Bool -> Bool -> Bool  -> IO CommanderTransmissionDonneesInfraJType
initType_ prod myPointId accordPersonneType sens getCDC getIDX getPTD = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let soutirage = case sens of
            SensSOUTIRAGE -> True
            SensINJECTION -> False

    let (accordBool, personTypeChoice) = case accordPersonneType of
            Nothing ->
                (False, OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString ""
                  , personnePhysiqueType_prenom = Nothing
                  })
            Just (AccordPersonnePhysiqueNom nomPhy) ->
                (True, OneOf2 $ PersonnePhysiqueType
                  { personnePhysiqueType_civilite = Nothing
                  , personnePhysiqueType_nom = Chaine255Type $ Xsd.XsdString nomPhy
                  , personnePhysiqueType_prenom = Nothing
                  })
            Just (AccordPersonneMoraleDenominationSociale denomi) ->
                (True, TwoOf2 $ PersonneMoraleType
                  { personneMoraleType_denominationSociale = Chaine255Type $ Xsd.XsdString denomi
                  })

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
              { declarationAccordClientType_accordClient = Ds.BooleenType accordBool
              , declarationAccordClientType_injection = Ds.BooleenType $ not soutirage
              , declarationAccordClientType_soutirage = Ds.BooleenType soutirage
              , declarationAccordClientType_choice3 = personTypeChoice
              }]
            , demandeAccesDonneesType_injection = Ds.BooleenType $ not soutirage
            , demandeAccesDonneesType_soutirage = Ds.BooleenType soutirage
            , demandeAccesDonneesType_cdc  = Ds.BooleenType getCDC
            , demandeAccesDonneesType_idx = Ds.BooleenType getIDX
            , demandeAccesDonneesType_ptd = Ds.BooleenType getPTD
            }
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE. 
initType :: String              -- ^ myPointId : point de référence sur lequel on souhaite obtenir des informations.
         -> Maybe AccordPersonneType -- ^ accordPersonneType : certifie l'accord du client et son type :
                                --
                                -- - Just PersonnePhysique : accord True, nom de la personne physique,
                                -- - Just PersonneMorale : accord True, dénomination morale,
                                -- - Nothing : accord False (cas non-recevable, ex. F375A-NR1 → SGT566).
         -> Sens                -- ^ sens : indique le sens de l’énergie circulant vers le réseau d’Enedis : 
                                -- 
                                -- - INJECTION,
                                -- - SOUTIRAGE. 
         -> Bool                -- ^ getCDC : précise si les données des courbes de charge et de la courbe de tension sont demandées.
         -> Bool                -- ^ getIDX : précise si les données d’index sont demandées.
         -> Bool                -- ^ getPTD : précise si les données de gestion de la tarification dynamique sont demandées.
         -> IO CommanderTransmissionDonneesInfraJType
initType = initType_ True

initTypeTest :: String -> Maybe AccordPersonneType -> Sens -> Bool -> Bool -> Bool  -> IO CommanderTransmissionDonneesInfraJType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
                       ( Just $ AccordPersonnePhysiqueNom (T.unpack $ nomClientFinalOuDenominationSociale testEnv) )
                       SensSOUTIRAGE False True False
    rep <- wsRequest myType :: IO (Either (String, String) CommanderTransmissionDonneesInfraJResponseType)
    pPrint rep 

data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)

data AccordPersonneType
    = AccordPersonnePhysiqueNom String
    | AccordPersonneMoraleDenominationSociale String
    deriving (Eq,Show)
