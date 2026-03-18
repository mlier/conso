{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest, Sens(..)
) where

import qualified Data.Text as T
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Type
    ( AdresseEmailType(AdresseEmailType),
      CommanderModificationOptionsServicesAccesDonneesResponseType,
      CommanderModificationOptionsServicesAccesDonneesType(..),
      ContratIdType(ContratIdType),
      DemandeType(DemandeType, demandeType_donneesGenerales,
                  demandeType_servicesSouscrits),
      DonneesGeneralesType(DonneesGeneralesType,
                           donneesGeneralesType_pointId,
                           donneesGeneralesType_initiateurLogin,
                           donneesGeneralesType_contratId,
                           donneesGeneralesType_sens),
      PointIdType(PointIdType),
      SensType(SensTypeSOUTIRAGE, SensTypeINJECTION),
      BooleenType(BooleenType),
      OptionPublicationType(OptionPublicationType,
                            optionPublicationType_mesuresCorrigees,
                            optionPublicationType_periodiciteTransmission),
      OptionsPublicationType(OptionsPublicationType),
      PeriodiciteTransmissionType(PeriodiciteTransmissionType),
      ServiceIdType(ServiceIdType),
      ServicesSouscritsType(ServicesSouscritsType),
      ServiceSouscritType(ServiceSouscritType,
                          serviceSouscritType_serviceSouscritId,
                          serviceSouscritType_ajouterOptionsPublication,
                          serviceSouscritType_supprimerOptionsPublication),
      elementCommanderModificationOptionsServicesAccesDonneesResponse,
      elementToXMLCommanderModificationOptionsServicesAccesDonnees )

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
      Test(pointId) )


instance RequestType CommanderModificationOptionsServicesAccesDonneesType where
  configReq = ConfigRequest{
                     urlSge = "/CommandeModificationOptionsServicesAccesDonnees/v1.0"
                   , soapAction = " "
                   , elementToXMLRequest = elementToXMLCommanderModificationOptionsServicesAccesDonnees
                   }

instance ResponseType CommanderModificationOptionsServicesAccesDonneesResponseType where
  configResp = ConfigResponse{
                    xmlTag = "commanderModificationOptionsServicesAccesDonneesResponse"
                   , elementResponse = elementCommanderModificationOptionsServicesAccesDonneesResponse
                   }


-- | Convertit une liste de (mesuresCorrigees, periodiciteTransmission) en OptionsPublicationType.
--   Liste vide → Nothing.
toOptionsType :: [(Maybe Bool, String)] -> Maybe OptionsPublicationType
toOptionsType [] = Nothing
toOptionsType opts = Just $ OptionsPublicationType
    [ OptionPublicationType
      { optionPublicationType_mesuresCorrigees = fmap BooleenType mc
      , optionPublicationType_periodiciteTransmission = PeriodiciteTransmissionType $ Xsd.XsdString p
      }
    | (mc, p) <- opts
    ]

initType_ :: Bool -> String -> Sens -> String -> [(Maybe Bool, String)] -> [(Maybe Bool, String)]
          -> IO CommanderModificationOptionsServicesAccesDonneesType
initType_ prod myPointId sens serviceId ajouterOptions supprimerOptions = do
    (loginUtilisateur, contratId) <- getLoginContrat prod

    let sensType = case sens of
            SensSOUTIRAGE -> SensTypeSOUTIRAGE
            SensINJECTION -> SensTypeINJECTION

    let requestType = CommanderModificationOptionsServicesAccesDonneesType{
          commanderModificationOptionsServicesAccesDonneesType_demande = DemandeType
          { demandeType_donneesGenerales = DonneesGeneralesType
            { donneesGeneralesType_pointId = PointIdType $ Xsd.XsdString myPointId
            , donneesGeneralesType_initiateurLogin = AdresseEmailType $ Xsd.XsdString loginUtilisateur
            , donneesGeneralesType_contratId = ContratIdType $ Xsd.XsdString contratId
            , donneesGeneralesType_sens = sensType
            }
          , demandeType_servicesSouscrits = ServicesSouscritsType
            [ ServiceSouscritType
              { serviceSouscritType_serviceSouscritId = ServiceIdType $ Xsd.XsdString serviceId
              , serviceSouscritType_ajouterOptionsPublication = toOptionsType ajouterOptions
              , serviceSouscritType_supprimerOptionsPublication = toOptionsType supprimerOptions
              }
            ]
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String               -- ^ myPointId : identifiant PRM du point sur lequel porte la demande.
         -> Sens                 -- ^ sens : indique le sens de l'énergie.
         -> String               -- ^ serviceId : identifiant du service à modifier.
         -> [(Maybe Bool, String)] -- ^ ajouterOptions : options à ajouter [(mesuresCorrigees, periodiciteTransmission)].
         -> [(Maybe Bool, String)] -- ^ supprimerOptions : options à supprimer [(mesuresCorrigees, periodiciteTransmission)].
         -> IO CommanderModificationOptionsServicesAccesDonneesType
initType = initType_ True

initTypeTest :: String -> Sens -> String -> [(Maybe Bool, String)] -> [(Maybe Bool, String)]
             -> IO CommanderModificationOptionsServicesAccesDonneesType
initTypeTest = initType_ False


myrequest :: IO ()
myrequest = do
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv)
                       SensSOUTIRAGE
                       "" [] []
    rep <- wsRequest myType :: IO (Either (String, String) CommanderModificationOptionsServicesAccesDonneesResponseType)
    pPrint rep

data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)
