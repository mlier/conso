{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesV3 (
  initType, initTypeTest, myrequest, wsRequest, xmlRequest, wsRequestTest, xmlRequestTest
) where

import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import qualified Data.Text as T
import           Text.Pretty.Simple (pPrint)

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType(PointIdType) )

import Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
    ( elementConsulterMesuresDetailleesResponseV3,
      elementToXMLConsulterMesuresDetailleesV3,
      CadreAccesType(CadreAccesTypeACCORDCLIENT),
      ConsulterMesuresDetailleesV3ResponseType,
      ConsulterMesuresDetailleesV3Type(..),
      Demande(Demande, demande_cadreAcces, demande_initiateurLogin,
              demande_pointId, demande_mesuresTypeCode, demande_grandeurPhysique,
              demande_dateDebut, demande_dateFin, demande_mesuresPas,
              demande_mesuresCorrigees, demande_sens),
      MesuresPasType,
      MesuresTypeCodeType(MesuresTypeCodeTypeINDEX),
      SensMesureType(SensMesureTypeSOUTIRAGE) )

import Conso.Fr.Elec.Sge.Sge
    ( ConfigRequest(ConfigRequest, elementToXMLRequest, urlSge,
                    soapAction),
      RequestType(..),
      ConfigResponse(ConfigResponse, elementResponse, xmlTag),
      ResponseType(..),
      SgeEnv(test),
      Test(pointId),
      wsRequest,
      xmlRequest,
      wsRequestTest,
      xmlRequestTest,
      getEnv,
      getLoginContrat )

   


instance RequestType ConsulterMesuresDetailleesV3Type where
  configReq = ConfigRequest{
                     urlSge = "/ConsultationMesuresDetaillees/v3.0"
                   , soapAction = "http://www.enedis.fr/sge/b2b/services/consultationmesuresdetaillees/v3.0"
                   , elementToXMLRequest = elementToXMLConsulterMesuresDetailleesV3
                   }

instance ResponseType ConsulterMesuresDetailleesV3ResponseType where
  configResp = ConfigResponse{
                     xmlTag = "consulterMesuresDetailleesResponseV3"
                   , elementResponse = elementConsulterMesuresDetailleesResponseV3
                   }
             

initType_ :: Bool -> String -> MesuresTypeCodeType -> String -> String -> String -> Maybe MesuresPasType -> 
            Bool -> SensMesureType -> CadreAccesType -> IO ConsulterMesuresDetailleesV3Type
initType_ prod myPointId mesuresTypeCode grandeurPhysique dateDebut dateFin 
         mesuresPas mesuresCorrigees sens cadreAcces = do
    (loginUtilisateur, _) <- getLoginContrat prod

    let requestType = ConsulterMesuresDetailleesV3Type{ 
          consulterMesuresDetailleesV3Type_demande = Demande {
                  demande_initiateurLogin = Xsd.XsdString loginUtilisateur
                , demande_pointId = PointIdType $ Xsd.XsdString myPointId
                , demande_mesuresTypeCode = mesuresTypeCode
                , demande_grandeurPhysique = Xsd.XsdString grandeurPhysique
                , demande_dateDebut = Xsd.Date dateDebut
                , demande_dateFin = Xsd.Date dateFin
                , demande_mesuresPas = mesuresPas
                , demande_mesuresCorrigees = mesuresCorrigees
                , demande_sens = sens
                , demande_cadreAcces = cadreAcces
          }
        }
    return requestType

-- | initType renvoit un objet de configuration utilisable par wsRequest sur le serveur de production de SGE.
initType :: String                -- ^ myPointId : point de référence sur lequel on souhaite obtenir des informations 
         -> MesuresTypeCodeType   -- ^ mesuresTypeCode peut prendre les valeurs suivantes :
                                  --
                                  -- - ENERGIE pour les énergies globales quotidiennes,
                                  -- - PMAX pour les puissances maximales quotidiennes ou mensuelles,
                                  -- - COURBE pour une courbe (de puissance ou de tension),
                                  -- - INDEX pour les index quotidiens.
         -> String                -- ^ grandeurPhysique : Grandeur physique demandée parmi les valeurs : 
                                  --
                                  -- - Pour une demande de courbe :
                                  --
                                  --     - PA pour récupérer les courbes de puissance active (seule courbe disponible 
                                  --       pour les segments C5 et P4),
                                  --     - PRI pour récupérer les courbes de puissance réactive inductive,
                                  --     - PRC pour récupérer les courbes de puissance réactive capacitive,
                                  --     - E pour récupérer les courbes de tension,
                                  --     - TOUT pour récupérer les courbes disponibles.
                                  --
                                  -- - Pour une demande de puissance maximale :
                                  --
                                  --     - PMA pour récupérer la puissance maximale d’un compteur monophasé ou la puissance 
                                  --       maximale 'équivalente monophasé' (Pmax de la somme des trois phases) pour un 
                                  --       compteur triphasé,
                                  --     - TOUT pour récupérer l’ensemble des données disponibles (puissance maximale pour un 
                                  --       compteurmonophasé ou la puissance maximale « équivalentemonophasé » et les 
                                  --       puissances maximales par phase pour un compteur triphasé).
                                  --
                                  -- - Pour une demande en énergie globale quotidienne :
                                  --
                                  --     - EA pour récupérer les données d’énergie active,
                                  --     - ERC pour récupérer les données d’énergie réactive capacitive,
                                  --     - ERI pour récupérer les données d’énergie réactive inductive.
                                  --
                                  -- - Pour une demande d’index :
                                  --
                                  --     - EA pour récupérer les données d’énergie active,
                                  --     - ER pour récupérer les données d’énergie réactive,
                                  --     - ERC pour récupérer les données d’énergie réactive capacitive,
                                  --     - ERI pour récupérer les données d’énergie réactive inductive,
                                  --     - DD pour récupérer la durée de dépassement,
                                  --     - DE pour récupérer le dépassement énergétique,
                                  --     - DQ pour récupérer le dépassement quadratique,
                                  --     - PMA pour récupérer la puissance maximale atteinte,
                                  --     - TF pour récupérer le temps de fonctionnement,
                                  --     - TOUT pour récupérer l’ensemble des données disponibles.
         -> String                -- ^ dateDebut : date de début souhaitée pour la consultation des mesures (date
                                  --   incluse), antérieure à la date de fin.
         -> String                -- ^ dateFin : date de fin souhaitée pour la consultation des mesures (date exclue).
         -> Maybe MesuresPasType  -- ^ mesuresPas : permet de définir le pas souhaité, uniquement pour la consultation 
                                  --   des puissances maximales quotidiennes et mensuelles :
                                  --
                                  -- - P1D pour un pas quotidien, 
                                  -- - P1M pour un pas mensuel.
         -> Bool                  -- ^ mesuresCorrigees : indique si le demandeur souhaite les mesures « BEST » ou non.
                                  --   Pour le C1-C4 et le P1-P3, pour le type de mesure COURBE uniquement, indiquer ‘true’ 
                                  --   pour des données « BEST », ‘false’ pour des données brutes. Il n’existe pas de 
                                  --   mécanisme de correction des mesures dans les autres cas (points C5 et P4 et/ou autre 
                                  --   type de mesure que COURBE). La balise doit donc être renseignée à ‘false’ dans ce cas.
         -> SensMesureType        -- ^ sens : Indique le sens de la mesure : 
                                  -- 
                                  -- - INJECTION,
                                  -- - SOUTIRAGE.
         -> CadreAccesType        -- ^ cadreAcces : indique à quel titre l’acteur consulte les données de mesures :
                                  --
                                  -- - ACCORD_CLIENT si le demandeur accède aux données de mesures au titre d’un accord du client,
                                  -- - SERVICE_ACCES si le demandeur a souscrit au préalable à un service d’accès aux données sur 
                                  --   le PRM,
                                  -- - EST_TITULAIRE si le demandeur est le fournisseur titulaire du contrat de fourniture sur le PRM.
                                  -- 
                                  --   Point d’attention : Seule la consultation de données de PMAX mensuelle est possible
                                  --   avec l’option "EST_TITULAIRE".

         -> IO ConsulterMesuresDetailleesV3Type
initType = initType_ True

initTypeTest :: String -> MesuresTypeCodeType -> String -> String -> String -> Maybe MesuresPasType -> 
            Bool -> SensMesureType -> CadreAccesType -> IO ConsulterMesuresDetailleesV3Type
initTypeTest = initType_ False 


myrequest :: IO()
myrequest = do 
    env <- getEnv
    let testEnv = test env
    myType <- initType (T.unpack $ pointId testEnv) MesuresTypeCodeTypeINDEX "EA" "2024-08-01" "2024-08-02" 
                       Nothing False SensMesureTypeSOUTIRAGE CadreAccesTypeACCORDCLIENT
    rep <- wsRequest myType :: IO (Either (String, String) ConsulterMesuresDetailleesV3ResponseType)
    --rep <- xmlRequest myType

    pPrint rep
