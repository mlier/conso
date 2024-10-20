{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.RechercherPointV20 where

import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString) )
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( Chaine255Type(Chaine255Type),
      AdresseAfnorLigneType(AdresseAfnorLigneType),
      CodePostalFrancaisType(CodePostalFrancaisType),
      CommuneFranceCodeInseeType(CommuneFranceCodeInseeType),
      AdresseEmailType(AdresseEmailType) )

import Conso.Fr.Elec.Sge.RechercherPointV20Type
    ( RechercherPointType(..),
      elementToXMLRechercherPoint,
      RechercherPointResponseType,
      AdresseInstallationType(AdresseInstallationType,
                              adresseInstallationType_codeInseeCommune,
                              adresseInstallationType_escalierEtEtageEtAppartement,
                              adresseInstallationType_batiment,
                              adresseInstallationType_numeroEtNomVoie,
                              adresseInstallationType_lieuDit,
                              adresseInstallationType_codePostal),
      elementRechercherPointResponse,
      CriteresType(CriteresType, criteresType_rechercheHorsPerimetre,
                   criteresType_adresseInstallation, criteresType_numSiret,
                   criteresType_matriculeOuNumeroSerie,
                   criteresType_domaineTensionAlimentationCode,
                   criteresType_nomClientFinalOuDenominationSociale,
                   criteresType_categorieClientFinalCode) )

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
      Test(codeInseeCommune, nomClientFinalOuDenominationSociale,
           numeroEtNomVoie, codePostal) )

    
instance RequestType RechercherPointType where
  configReq = ConfigRequest{
                     urlSge = "/RecherchePoint/v2.0"
                   , soapAction = "nimportequoimaispasvide"
                   , elementToXMLRequest = elementToXMLRechercherPoint
                   }

instance ResponseType RechercherPointResponseType where
  configResp = ConfigResponse{
                     xmlTag = "ns1:rechercherPointResponse" 
                   , elementResponse = elementRechercherPointResponse
                   }


initType_ :: Bool -> String -> String -> String -> String -> Bool -> IO RechercherPointType
initType_ prod myNomClientFinalOuDenominationSociale myNumeroEtNomVoie myCodePostal myCodeInseeCommune rechercheHorsPerimetre = do
    (loginUtilisateur, _) <- getLoginContrat prod

    let requestType = RechercherPointType
            { rechercherPointType_criteres = CriteresType
                { criteresType_adresseInstallation = Just AdresseInstallationType
                    { adresseInstallationType_escalierEtEtageEtAppartement = Nothing
                    , adresseInstallationType_batiment = Nothing
                    , adresseInstallationType_numeroEtNomVoie = Just $ Ds.AdresseAfnorLigneType $ XsdString myNumeroEtNomVoie
                    , adresseInstallationType_lieuDit = Nothing
                    , adresseInstallationType_codePostal = Just $ Ds.CodePostalFrancaisType $ XsdString myCodePostal
                    , adresseInstallationType_codeInseeCommune = Just $ Ds.CommuneFranceCodeInseeType $ XsdString myCodeInseeCommune
                    }
                , criteresType_numSiret = Nothing
                , criteresType_matriculeOuNumeroSerie = Nothing
                , criteresType_domaineTensionAlimentationCode = Nothing
                , criteresType_nomClientFinalOuDenominationSociale = Just $ Ds.Chaine255Type $ XsdString myNomClientFinalOuDenominationSociale
                , criteresType_categorieClientFinalCode = Nothing
                , criteresType_rechercheHorsPerimetre = Just rechercheHorsPerimetre
                }
                , rechercherPointType_loginUtilisateur = Ds.AdresseEmailType $ XsdString loginUtilisateur
            }
    return requestType

initType :: String -> String -> String -> String -> Bool -> IO RechercherPointType
initType = initType_ True

initTypeTest :: String -> String -> String -> String -> Bool -> IO RechercherPointType
initTypeTest = initType_ False


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ nomClientFinalOuDenominationSociale testEnv) (T.unpack $ numeroEtNomVoie testEnv) 
                        (T.unpack $ codePostal testEnv) (T.unpack $ codeInseeCommune testEnv) True
    rep <- wsRequest myType :: IO ( Either (String, String) RechercherPointResponseType )
    pPrint rep
