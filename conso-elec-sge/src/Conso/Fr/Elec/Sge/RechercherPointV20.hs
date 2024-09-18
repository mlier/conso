{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.RechercherPointV20 where

import qualified Data.Text as T
import           Text.XML.HaXml.Schema.PrimitiveTypes ( XsdString(XsdString) )
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( CommuneFranceCodeInseeType(CommuneFranceCodeInseeType),
      CodePostalFrancaisType(CodePostalFrancaisType),
      Chaine255Type(Chaine255Type),
      AdresseEmailType(AdresseEmailType),
      AdresseAfnorLigneType(AdresseAfnorLigneType) )
import Conso.Fr.Elec.Sge.RechercherPointV20Type
    ( AdresseInstallationType(AdresseInstallationType,
                              adresseInstallationType_codeInseeCommune,
                              adresseInstallationType_escalierEtEtageEtAppartement,
                              adresseInstallationType_batiment,
                              adresseInstallationType_numeroEtNomVoie,
                              adresseInstallationType_lieuDit,
                              adresseInstallationType_codePostal),
      CriteresType(CriteresType, criteresType_rechercheHorsPerimetre,
                   criteresType_adresseInstallation, criteresType_numSiret,
                   criteresType_matriculeOuNumeroSerie,
                   criteresType_domaineTensionAlimentationCode,
                   criteresType_nomClientFinalOuDenominationSociale,
                   criteresType_categorieClientFinalCode),
      RechercherPointResponseType,
      RechercherPointType(..),
      elementToXMLRechercherPoint,
      elementRechercherPointResponse )
import Conso.Fr.Elec.Sge.Sge
    ( ResponseType,
      RequestType,
      ConfigWS(ConfigWS, elementResponse, urlSge, soapAction,
               elementToXMLRequest, xmlTag),
      Test(codeInseeCommune, nomClientFinalOuDenominationSociale,
           numeroEtNomVoie, codePostal),
      Sge(userB2b),
      Env(test, sge),
      getEnv,
      sgeRequest )

    

instance RequestType RechercherPointType
instance ResponseType RechercherPointResponseType


initType :: String -> String -> String -> String -> Bool -> IO RechercherPointType
initType myNomClientFinalOuDenominationSociale myNumeroEtNomVoie myCodePostal myCodeInseeCommune rechercheHorsPerimetre = do
    env <- getEnv
    let sgeEnv = sge env
    let loginUtilisateur = userB2b sgeEnv

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
                , rechercherPointType_loginUtilisateur = Ds.AdresseEmailType $ XsdString $ T.unpack loginUtilisateur
            }
    return requestType


wsRequest :: RechercherPointType -> IO ()
wsRequest r = sgeRequest r configWS
    where configWS = ConfigWS{
                          urlSge = "/RecherchePoint/v2.0"
                        , soapAction = "nimportequoimaispasvide"
                        , elementToXMLRequest = elementToXMLRechercherPoint
                        , xmlTag = "ns1:rechercherPointResponse" 
                        , elementResponse = elementRechercherPointResponse
    }

myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType  (T.unpack $ nomClientFinalOuDenominationSociale testEnv) (T.unpack $ numeroEtNomVoie testEnv) 
                        (T.unpack $ codePostal testEnv) (T.unpack $ codeInseeCommune testEnv) True
    wsRequest myType
    pPrint myType
