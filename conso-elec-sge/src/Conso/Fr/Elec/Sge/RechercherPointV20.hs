{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Conso.Fr.Elec.Sge.RechercherPointV20
Description : Webservice B2B RecherchePoint v2.0 (Enedis.SGE.GUI.0427 v1.2.0)

Permet de rechercher un point de livraison (PRM) à partir de critères d'adresse,
de SIRET, de matricule compteur, ou de tension d'alimentation.

Par défaut la recherche est limitée au périmètre fournisseur.
@rechercheHorsPerimetre = Just True@ permet de chercher hors périmètre
en fournissant obligatoirement l'adresse exacte et le nom du client.
-}
module Conso.Fr.Elec.Sge.RechercherPointV20 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

import qualified Data.Text as T

import Text.XML.HaXml.Schema.PrimitiveTypes 

--import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( AdresseAfnorLigneType(AdresseAfnorLigneType),
      AdresseEmailType(AdresseEmailType),
      Chaine255Type(Chaine255Type),
      ClientFinalCategorieCodeType,
      CodePostalFrancaisType(CodePostalFrancaisType),
      CommuneFranceCodeInseeType(CommuneFranceCodeInseeType),
      DomaineTensionCodeType,
      EtablissementNumSiretType(EtablissementNumSiretType) )
  

import Conso.Fr.Elec.Sge.RechercherPointV20Type
    ( elementRechercherPointResponse,
      elementToXMLRechercherPoint,
      AdresseInstallationType(AdresseInstallationType,
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
      RechercherPointType(..) )


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
                     xmlTag = "rechercherPointResponse"
                   , elementResponse = elementRechercherPointResponse
                   }


initType_ :: Bool -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
          -> Maybe String -> Maybe String -> Maybe DomaineTensionCodeType 
          -> Maybe String -> Maybe ClientFinalCategorieCodeType -> Maybe Bool
          -> IO RechercherPointType
initType_ prod myEscEtaAppart myBatiment myNumeroEtNomVoie myLieuDit myCodePostal myCodeInseeCommune
              myNumSiret myMatriculeNumeroSerie myDomaineTensionAlimentation
              myNomClientFinalOuDenominationSociale myCategorieClientFinalCode rechercheHorsPerimetre = do
    (loginUtilisateur, _) <- getLoginContrat prod

    let requestType = RechercherPointType
            { rechercherPointType_criteres = CriteresType
                { criteresType_adresseInstallation = Just AdresseInstallationType
                    { adresseInstallationType_escalierEtEtageEtAppartement = Ds.AdresseAfnorLigneType . XsdString <$> myEscEtaAppart
                    , adresseInstallationType_batiment = Ds.AdresseAfnorLigneType . XsdString <$> myBatiment
                    , adresseInstallationType_numeroEtNomVoie = Ds.AdresseAfnorLigneType . XsdString <$> myNumeroEtNomVoie
                    , adresseInstallationType_lieuDit = Ds.AdresseAfnorLigneType . XsdString <$> myLieuDit
                    , adresseInstallationType_codePostal = Ds.CodePostalFrancaisType . XsdString <$> myCodePostal
                    , adresseInstallationType_codeInseeCommune = Ds.CommuneFranceCodeInseeType . XsdString <$> myCodeInseeCommune
                    }
                , criteresType_numSiret = Ds.EtablissementNumSiretType . XsdString <$> myNumSiret
                , criteresType_matriculeOuNumeroSerie = Ds.Chaine255Type . XsdString <$> myMatriculeNumeroSerie
                , criteresType_domaineTensionAlimentationCode = myDomaineTensionAlimentation
                , criteresType_nomClientFinalOuDenominationSociale = Ds.Chaine255Type . XsdString <$> myNomClientFinalOuDenominationSociale
                , criteresType_categorieClientFinalCode = myCategorieClientFinalCode
                , criteresType_rechercheHorsPerimetre = rechercheHorsPerimetre
                }
                , rechercherPointType_loginUtilisateur = Ds.AdresseEmailType $ XsdString loginUtilisateur
            }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: Maybe String   -- ^ myEscEtaAppart : Escalier, étage et numéro d’appartement de l’adresse du point.
         -> Maybe String   -- ^ myBatiment : Contient les informations sur l’entrée, le bâtiment, la tour, 
                           --   l’immeuble, la résidence de l’adresse du point.
         -> Maybe String   -- ^ myNumeroEtNomVoie : Numéro et libellé de voie de l’adresse du point.
         -> Maybe String   -- ^ myLieuDit : Lieu-dit de l’adresse du point.
         -> Maybe String   -- ^ myCodePostal : Code postal de l’adresse du point.
         -> Maybe String   -- ^ myCodeInseeCommune : Code INSEE de la commune de l’adresse du point.
         -> Maybe String   -- ^ myNumSiret : Numéro de SIRET du client final.
         -> Maybe String   -- ^ myMatriculeNumeroSerie : Matricule du compteur en C5 ou numéro de série du compteur en C2-C4.
         -> Maybe DomaineTensionCodeType  -- ^ myDomaineTensionAlimentation : Domaine de tension de l’alimentation principale 
                           --   du point (BTINF, BTSUP, HTA, HTB).	 
         -> Maybe String   -- ^ myNomClientFinalOuDenominationSociale : Nom ou dénomination sociale du client final.
         -> Maybe ClientFinalCategorieCodeType  -- ^ myCategorieClientFinalCode : Catégorie du client final (PROfessionel ou 
                           --   RESidentiel).
         -> Maybe Bool     -- ^ rechercheHorsPerimetre : Booléen permettant au demandeur, qui n’est pas fournisseur titulaire 
                           --   du point, de rechercher un point en service en précisant les trois critères suivants :
                           --
                           -- - adresse et communce exactes du client.
                           -- - nom/dénomination sociale du client exact ou partiel avec un minimum de 3 caractères.
         -> IO RechercherPointType
initType = initType_ True

-- | Comme 'initType' mais sur le serveur d'homologation.
initTypeTest :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
          -> Maybe String -> Maybe String -> Maybe DomaineTensionCodeType
          -> Maybe String -> Maybe ClientFinalCategorieCodeType -> Maybe Bool
          -> IO RechercherPointType
initTypeTest = initType_ False


-- | Exemple d'appel en production avec l'adresse de test configurée dans le fichier YAML.
myrequest :: IO()
myrequest = do
    env <- getEnv
    let testEnv = test env
    let 
    myType <- initType Nothing Nothing 
                ( Just (T.unpack $ numeroEtNomVoie testEnv) )
                Nothing 
                (Just (T.unpack $ codePostal testEnv) )
                (Just (T.unpack $ codeInseeCommune testEnv) )
                Nothing Nothing Nothing 
                (Just (T.unpack $ nomClientFinalOuDenominationSociale testEnv)) 
                Nothing (Just True)
    rep <- wsRequest myType :: IO ( Either (String, String) RechercherPointResponseType )
    pPrint rep

