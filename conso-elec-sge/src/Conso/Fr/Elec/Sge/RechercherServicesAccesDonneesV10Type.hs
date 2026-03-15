{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
  ( module Conso.Fr.Elec.Sge.RechercherServicesAccesDonneesV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data CriteresType = CriteresType
        { criteresType_pointId :: [Ds.PointIdType]
        , criteresType_contratId :: [Ds.ContratIdType]
        }
        deriving (Eq,Show)
instance SchemaType CriteresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CriteresType
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "pointId")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "contratId")
    schemaTypeToXML s x@CriteresType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "pointId") $ criteresType_pointId x
            , concatMap (schemaTypeToXML "contratId") $ criteresType_contratId x
            ]
 
elementRechercherServicesAccesDonnees :: XMLParser RechercherServicesAccesDonneesType
elementRechercherServicesAccesDonnees = parseSchemaType "rechercherServicesAccesDonnees"
elementToXMLRechercherServicesAccesDonnees :: RechercherServicesAccesDonneesType -> [Content ()]
elementToXMLRechercherServicesAccesDonnees = schemaTypeToXML "rechercherServicesAccesDonnees"
 
elementRechercherServicesAccesDonneesReponse :: XMLParser RechercherServicesAccesDonneesReponseType
elementRechercherServicesAccesDonneesReponse = parseSchemaType "rechercherServicesAccesDonneesReponse"
elementToXMLRechercherServicesAccesDonneesReponse :: RechercherServicesAccesDonneesReponseType -> [Content ()]
elementToXMLRechercherServicesAccesDonneesReponse = schemaTypeToXML "rechercherServicesAccesDonneesReponse"
 
newtype RechercherServicesAccesDonneesReponseType = RechercherServicesAccesDonneesReponseType
        { rechercherServicesAccesDonneesReponseType_servicesSouscrits :: Maybe ServicesSouscritsType
        }
        deriving (Eq,Show)
instance SchemaType RechercherServicesAccesDonneesReponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RechercherServicesAccesDonneesReponseType
            `apply` optional (parseSchemaType "servicesSouscrits")
    schemaTypeToXML s x@RechercherServicesAccesDonneesReponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "servicesSouscrits") $ rechercherServicesAccesDonneesReponseType_servicesSouscrits x
            ]
 
data RechercherServicesAccesDonneesType = RechercherServicesAccesDonneesType
        { rechercherServicesAccesDonneesType_criteres :: [CriteresType]
        , rechercherServicesAccesDonneesType_loginUtilisateur :: [Ds.UtilisateurLoginType]
        }
        deriving (Eq,Show)
instance SchemaType RechercherServicesAccesDonneesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RechercherServicesAccesDonneesType
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "criteres")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "loginUtilisateur")
    schemaTypeToXML s x@RechercherServicesAccesDonneesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "criteres") $ rechercherServicesAccesDonneesType_criteres x
            , concatMap (schemaTypeToXML "loginUtilisateur") $ rechercherServicesAccesDonneesType_loginUtilisateur x
            ]
 
data ServiceSouscritType = ServiceSouscritType
        { serviceSouscritType_serviceSouscritId :: Ds.Chaine15Type
        , serviceSouscritType_pointId :: Ds.PointIdType
        , serviceSouscritType_serviceSouscritCode :: Ds.Chaine255Type
        , serviceSouscritType_serviceSouscritLibelle :: Ds.Chaine255Type
        , serviceSouscritType_injection :: [Ds.BooleenType]
        , serviceSouscritType_soutirage :: [Ds.BooleenType]
        , serviceSouscritType_contratId :: Maybe Ds.ContratIdType
        , serviceSouscritType_contratLibelle :: Maybe Ds.Chaine255Type
        , serviceSouscritType_etatCode :: [Ds.Chaine15Type]
        , serviceSouscritType_dateDebut :: [Ds.DateType]
        , serviceSouscritType_dateFin :: Maybe Ds.DateType
        , serviceSouscritType_motifFinLibelle :: Maybe Ds.Chaine255Type
        , serviceSouscritType_mesuresTypeCode :: Maybe Ds.MesureTypeCodeType
        , serviceSouscritType_mesuresPas :: Maybe Ds.Chaine15Type
        , serviceSouscritType_optionsPublication :: Maybe OptionsPublicationType
        }
        deriving (Eq,Show)
instance SchemaType ServiceSouscritType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceSouscritType
            `apply` parseSchemaType "serviceSouscritId"
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "serviceSouscritCode"
            `apply` parseSchemaType "serviceSouscritLibelle"
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "injection")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "soutirage")
            `apply` optional (parseSchemaType "contratId")
            `apply` optional (parseSchemaType "contratLibelle")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "etatCode")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "dateDebut")
            `apply` optional (parseSchemaType "dateFin")
            `apply` optional (parseSchemaType "motifFinLibelle")
            `apply` optional (parseSchemaType "mesuresTypeCode")
            `apply` optional (parseSchemaType "mesuresPas")
            `apply` optional (parseSchemaType "optionsPublication")
    schemaTypeToXML s x@ServiceSouscritType{} =
        toXMLElement s []
            [ schemaTypeToXML "serviceSouscritId" $ serviceSouscritType_serviceSouscritId x
            , schemaTypeToXML "pointId" $ serviceSouscritType_pointId x
            , schemaTypeToXML "serviceSouscritCode" $ serviceSouscritType_serviceSouscritCode x
            , schemaTypeToXML "serviceSouscritLibelle" $ serviceSouscritType_serviceSouscritLibelle x
            , concatMap (schemaTypeToXML "injection") $ serviceSouscritType_injection x
            , concatMap (schemaTypeToXML "soutirage") $ serviceSouscritType_soutirage x
            , maybe [] (schemaTypeToXML "contratId") $ serviceSouscritType_contratId x
            , maybe [] (schemaTypeToXML "contratLibelle") $ serviceSouscritType_contratLibelle x
            , concatMap (schemaTypeToXML "etatCode") $ serviceSouscritType_etatCode x
            , concatMap (schemaTypeToXML "dateDebut") $ serviceSouscritType_dateDebut x
            , maybe [] (schemaTypeToXML "dateFin") $ serviceSouscritType_dateFin x
            , maybe [] (schemaTypeToXML "motifFinLibelle") $ serviceSouscritType_motifFinLibelle x
            , maybe [] (schemaTypeToXML "mesuresTypeCode") $ serviceSouscritType_mesuresTypeCode x
            , maybe [] (schemaTypeToXML "mesuresPas") $ serviceSouscritType_mesuresPas x
            , maybe [] (schemaTypeToXML "optionsPublication") $ serviceSouscritType_optionsPublication x
            ]
 
newtype OptionsPublicationType = OptionsPublicationType
        { optionsPublicationType_optionPublication :: [OptionPublicationType]
        }
        deriving (Eq,Show)
instance SchemaType OptionsPublicationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return OptionsPublicationType
            `apply` many1 (parseSchemaType "optionPublication")
    schemaTypeToXML s x@OptionsPublicationType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "optionPublication") $ optionsPublicationType_optionPublication x
            ]
 
data OptionPublicationType = OptionPublicationType
        { optionPublicationType_mesuresCorrigees :: Maybe BooleenType
        , optionPublicationType_periodiciteTransmission :: PeriodiciteTransmissionType
        , optionPublicationType_dateDebut :: DateType
        , optionPublicationType_dateFin :: Maybe DateType
        }
        deriving (Eq,Show)
instance SchemaType OptionPublicationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return OptionPublicationType
            `apply` optional (parseSchemaType "mesuresCorrigees")
            `apply` parseSchemaType "periodiciteTransmission"
            `apply` parseSchemaType "dateDebut"
            `apply` optional (parseSchemaType "dateFin")
    schemaTypeToXML s x@OptionPublicationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "mesuresCorrigees") $ optionPublicationType_mesuresCorrigees x
            , schemaTypeToXML "periodiciteTransmission" $ optionPublicationType_periodiciteTransmission x
            , schemaTypeToXML "dateDebut" $ optionPublicationType_dateDebut x
            , maybe [] (schemaTypeToXML "dateFin") $ optionPublicationType_dateFin x
            ]
 
newtype ServicesSouscritsType = ServicesSouscritsType
        { servicesSouscritsType_serviceSouscrit :: [ServiceSouscritType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesSouscritsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesSouscritsType
            `apply` between (Occurs (Just 0) (Just 200))
                            (parseSchemaType "serviceSouscrit")
    schemaTypeToXML s x@ServicesSouscritsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceSouscrit") $ servicesSouscritsType_serviceSouscrit x
            ]
 
newtype PeriodiciteTransmissionType = PeriodiciteTransmissionType Xs.XsdString deriving (Eq,Show)
instance Restricts PeriodiciteTransmissionType Xs.XsdString where
    restricts (PeriodiciteTransmissionType x) = x
instance SchemaType PeriodiciteTransmissionType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PeriodiciteTransmissionType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PeriodiciteTransmissionType where
    acceptingParser = fmap PeriodiciteTransmissionType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PeriodiciteTransmissionType x) = simpleTypeText x
 

