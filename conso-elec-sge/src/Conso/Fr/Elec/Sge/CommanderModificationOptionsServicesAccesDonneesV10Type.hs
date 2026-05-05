{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-|
Module      : Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Type
Description : Types Haskell générés depuis le WSDL CommandeModificationOptionsServicesAccesDonnees v1.0

Généré automatiquement depuis le fichier WSDL Enedis par HaXml.
Ne pas modifier manuellement.
-}
module Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Type
  ( module Conso.Fr.Elec.Sge.CommanderModificationOptionsServicesAccesDonneesV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderModificationOptionsServicesAccesDonnees :: XMLParser CommanderModificationOptionsServicesAccesDonneesType
elementCommanderModificationOptionsServicesAccesDonnees = parseSchemaType "commanderModificationOptionsServicesAccesDonnees"
elementToXMLCommanderModificationOptionsServicesAccesDonnees :: CommanderModificationOptionsServicesAccesDonneesType -> [Content ()]
elementToXMLCommanderModificationOptionsServicesAccesDonnees = schemaTypeToXML "sc:commanderModificationOptionsServicesAccesDonnees"
 
elementCommanderModificationOptionsServicesAccesDonneesResponse :: XMLParser CommanderModificationOptionsServicesAccesDonneesResponseType
elementCommanderModificationOptionsServicesAccesDonneesResponse = parseSchemaType "commanderModificationOptionsServicesAccesDonneesResponse"
elementToXMLCommanderModificationOptionsServicesAccesDonneesResponse :: CommanderModificationOptionsServicesAccesDonneesResponseType -> [Content ()]
elementToXMLCommanderModificationOptionsServicesAccesDonneesResponse = schemaTypeToXML "commanderModificationOptionsServicesAccesDonneesResponse"
 
newtype CommanderModificationOptionsServicesAccesDonneesType = CommanderModificationOptionsServicesAccesDonneesType
        { commanderModificationOptionsServicesAccesDonneesType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderModificationOptionsServicesAccesDonneesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderModificationOptionsServicesAccesDonneesType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderModificationOptionsServicesAccesDonneesType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/ws/commanderModificationOptionsServicesAccesDonnees/v1.0"
                       ]
            [ schemaTypeToXML "demande" $ commanderModificationOptionsServicesAccesDonneesType_demande x
            ]
 
newtype CommanderModificationOptionsServicesAccesDonneesResponseType = CommanderModificationOptionsServicesAccesDonneesResponseType
        { commanderModificationOptionsServicesAccesDonneesResponseType_servicesModifies :: Maybe ServicesModifiesType
        }
        deriving (Eq,Show)
instance SchemaType CommanderModificationOptionsServicesAccesDonneesResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderModificationOptionsServicesAccesDonneesResponseType
            `apply` optional (parseSchemaType "servicesModifies")
    schemaTypeToXML s x@CommanderModificationOptionsServicesAccesDonneesResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "servicesModifies") $ commanderModificationOptionsServicesAccesDonneesResponseType_servicesModifies x
            ]
 
newtype ServicesModifiesType = ServicesModifiesType
        { servicesModifiesType_serviceSouscritId :: [ServiceIdType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesModifiesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesModifiesType
            `apply` between (Occurs (Just 1) (Just 100000))
                            (parseSchemaType "serviceSouscritId")
    schemaTypeToXML s x@ServicesModifiesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceSouscritId") $ servicesModifiesType_serviceSouscritId x
            ]
 
data DemandeType = DemandeType
        { demandeType_donneesGenerales :: DonneesGeneralesType
        , demandeType_servicesSouscrits :: ServicesSouscritsType
        }
        deriving (Eq,Show)
instance SchemaType DemandeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandeType
            `apply` parseSchemaType "donneesGenerales"
            `apply` parseSchemaType "servicesSouscrits"
    schemaTypeToXML s x@DemandeType{} =
        toXMLElement s []
            [ schemaTypeToXML "donneesGenerales" $ demandeType_donneesGenerales x
            , schemaTypeToXML "servicesSouscrits" $ demandeType_servicesSouscrits x
            ]
 
data DonneesGeneralesType = DonneesGeneralesType
        { donneesGeneralesType_pointId :: PointIdType
        , donneesGeneralesType_initiateurLogin :: UtilisateurLoginType
        , donneesGeneralesType_contratId :: ContratIdType
        , donneesGeneralesType_sens :: SensType
        }
        deriving (Eq,Show)
instance SchemaType DonneesGeneralesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DonneesGeneralesType
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "contratId"
            `apply` parseSchemaType "sens"
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGeneralesType_contratId x
            , schemaTypeToXML "sens" $ donneesGeneralesType_sens x
            ]
 
newtype ServicesSouscritsType = ServicesSouscritsType
        { servicesSouscritsType_serviceSouscrit :: [ServiceSouscritType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesSouscritsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesSouscritsType
            `apply` many1 (parseSchemaType "serviceSouscrit")
    schemaTypeToXML s x@ServicesSouscritsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceSouscrit") $ servicesSouscritsType_serviceSouscrit x
            ]
 
data ServiceSouscritType = ServiceSouscritType
        { serviceSouscritType_serviceSouscritId :: ServiceIdType
        , serviceSouscritType_ajouterOptionsPublication :: Maybe OptionsPublicationType
        , serviceSouscritType_supprimerOptionsPublication :: Maybe OptionsPublicationType
        }
        deriving (Eq,Show)
instance SchemaType ServiceSouscritType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceSouscritType
            `apply` parseSchemaType "serviceSouscritId"
            `apply` optional (parseSchemaType "ajouterOptionsPublication")
            `apply` optional (parseSchemaType "supprimerOptionsPublication")
    schemaTypeToXML s x@ServiceSouscritType{} =
        toXMLElement s []
            [ schemaTypeToXML "serviceSouscritId" $ serviceSouscritType_serviceSouscritId x
            , maybe [] (schemaTypeToXML "ajouterOptionsPublication") $ serviceSouscritType_ajouterOptionsPublication x
            , maybe [] (schemaTypeToXML "supprimerOptionsPublication") $ serviceSouscritType_supprimerOptionsPublication x
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
        }
        deriving (Eq,Show)
instance SchemaType OptionPublicationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return OptionPublicationType
            `apply` optional (parseSchemaType "mesuresCorrigees")
            `apply` parseSchemaType "periodiciteTransmission"
    schemaTypeToXML s x@OptionPublicationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "mesuresCorrigees") $ optionPublicationType_mesuresCorrigees x
            , schemaTypeToXML "periodiciteTransmission" $ optionPublicationType_periodiciteTransmission x
            ]
 
newtype ServiceIdType = ServiceIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts ServiceIdType Xsd.XsdString where
    restricts (ServiceIdType x) = x
instance SchemaType ServiceIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ServiceIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ServiceIdType where
    acceptingParser = fmap ServiceIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ServiceIdType x) = simpleTypeText x
 
newtype BooleenType = BooleenType Xsd.Boolean deriving (Eq,Show)
instance Restricts BooleenType Xsd.Boolean where
    restricts (BooleenType x) = x
instance SchemaType BooleenType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (BooleenType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType BooleenType where
    acceptingParser = fmap BooleenType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (BooleenType x) = simpleTypeText x
 
newtype ContratIdType = ContratIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts ContratIdType Xsd.XsdString where
    restricts (ContratIdType x) = x
instance SchemaType ContratIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ContratIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ContratIdType where
    acceptingParser = fmap ContratIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ContratIdType x) = simpleTypeText x
 
data SensType = SensTypeSOUTIRAGE | SensTypeINJECTION deriving (Eq,Show,Enum)
instance SchemaType SensType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType SensType where
    acceptingParser =  do _ <- literal "SOUTIRAGE"; return SensTypeSOUTIRAGE
                      `onFail` do _ <- literal "INJECTION"; return SensTypeINJECTION
                      
    simpleTypeText SensTypeSOUTIRAGE = "SOUTIRAGE"
    simpleTypeText SensTypeINJECTION = "INJECTION"
 
newtype PointIdType = PointIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts PointIdType Xsd.XsdString where
    restricts (PointIdType x) = x
instance SchemaType PointIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PointIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PointIdType where
    acceptingParser = fmap PointIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{14})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (PointIdType x) = simpleTypeText x
 
newtype PeriodiciteTransmissionType = PeriodiciteTransmissionType Xsd.XsdString deriving (Eq,Show)
instance Restricts PeriodiciteTransmissionType Xsd.XsdString where
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
 
type UtilisateurLoginType = AdresseEmailType
-- Placeholder for a Union type, not yet implemented.
 
newtype UtilisateurNniType = UtilisateurNniType Xsd.XsdString deriving (Eq,Show)
instance Restricts UtilisateurNniType Xsd.XsdString where
    restricts (UtilisateurNniType x) = x
instance SchemaType UtilisateurNniType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (UtilisateurNniType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType UtilisateurNniType where
    acceptingParser = fmap UtilisateurNniType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{6}([A-Z0-9]{2})?)
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (UtilisateurNniType x) = simpleTypeText x
 
newtype AdresseEmailType = AdresseEmailType Xsd.XsdString deriving (Eq,Show)
instance Restricts AdresseEmailType Xsd.XsdString where
    restricts (AdresseEmailType x) = x
instance SchemaType AdresseEmailType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AdresseEmailType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AdresseEmailType where
    acceptingParser = fmap AdresseEmailType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9a-zA-Z][\-._0-9a-zA-Z]{0,255}@[0-9a-zA-Z][\-._0-9a-zA-Z]{0,255}.[a-zA-Z]{2,63})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (AdresseEmailType x) = simpleTypeText x
