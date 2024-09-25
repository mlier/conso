{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances  #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE InstanceSigs #-}

module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
  ( module Conso.Fr.Elec.Sge.DemandePublicationMesuresFinesM23V10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( Restricts(..),
      SchemaType(..),
      SimpleType(..),
      XMLParser,
      Content,
      between,
      parseSimpleType,
      toXMLAttribute,
      toXMLElement,
      toXMLText,
      element,
      interior,
      posnElement,
      optional,
      literal,
      apply,
      onFail,
      Occurs(Occurs),
      Commitment(commit) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementAffaireId :: XMLParser AffaireId
elementAffaireId = parseSchemaType "v1:affaireId"
elementToXMLAffaireId :: AffaireId -> [Content ()]
elementToXMLAffaireId = schemaTypeToXML "v1:affaireId"
 
elementDemandePublicationMesuresFines :: XMLParser DemandePublicationMesuresFines
elementDemandePublicationMesuresFines = parseSchemaType "sc:demandePublicationMesuresFines"
elementToXMLDemandePublicationMesuresFines :: DemandePublicationMesuresFines -> [Content ()]
elementToXMLDemandePublicationMesuresFines = schemaTypeToXML "sc:demandePublicationMesuresFines"
 
newtype AffaireId = AffaireId Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireId Xsd.XsdString where
    restricts (AffaireId x) = x
instance SchemaType AffaireId where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e  parseSimpleType
    schemaTypeToXML s (AffaireId x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireId where
    acceptingParser = fmap AffaireId acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireId x) = simpleTypeText x
 
data DemandePublicationMesuresFines = DemandePublicationMesuresFines
        { demandePublicationMesuresFines_donneesGenerales :: DonneesGenerales
        , demandePublicationMesuresFines_demande :: Demande
        }
        deriving (Eq,Show)
instance SchemaType DemandePublicationMesuresFines where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandePublicationMesuresFines
            `apply` parseSchemaType "donneesGenerales"
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@DemandePublicationMesuresFines{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "https://sge-b2b.enedis.fr/services/commandehistoriquedonneesmesuresfines/v1" 
                       ]
            [ schemaTypeToXML "donneesGenerales" $ demandePublicationMesuresFines_donneesGenerales x
            , schemaTypeToXML "demande" $ demandePublicationMesuresFines_demande x
            ]
 
newtype InitiateurLogin = InitiateurLogin Xsd.XsdString deriving (Eq,Show)
instance Restricts InitiateurLogin Xsd.XsdString where
    restricts (InitiateurLogin x) = x
instance SchemaType InitiateurLogin where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e  parseSimpleType
    schemaTypeToXML s (InitiateurLogin x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InitiateurLogin where
    acceptingParser = fmap InitiateurLogin acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (InitiateurLogin x) = simpleTypeText x
 
newtype ContratId = ContratId Xsd.XsdString deriving (Eq,Show)
instance Restricts ContratId Xsd.XsdString where
    restricts (ContratId x) = x
instance SchemaType ContratId where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e  parseSimpleType
    schemaTypeToXML s (ContratId x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ContratId where
    acceptingParser = fmap ContratId acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ContratId x) = simpleTypeText x
 
newtype ReferenceDemandeur = ReferenceDemandeur Xsd.XsdString deriving (Eq,Show)
instance Restricts ReferenceDemandeur Xsd.XsdString where
    restricts (ReferenceDemandeur x) = x
instance SchemaType ReferenceDemandeur where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e  parseSimpleType
    schemaTypeToXML s (ReferenceDemandeur x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReferenceDemandeur where
    acceptingParser = fmap ReferenceDemandeur acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReferenceDemandeur x) = simpleTypeText x
 
newtype ReferenceRegroupement = ReferenceRegroupement Xsd.XsdString deriving (Eq,Show)
instance Restricts ReferenceRegroupement Xsd.XsdString where
    restricts (ReferenceRegroupement x) = x
instance SchemaType ReferenceRegroupement where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e  parseSimpleType
    schemaTypeToXML s (ReferenceRegroupement x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReferenceRegroupement where
    acceptingParser = fmap ReferenceRegroupement acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReferenceRegroupement x) = simpleTypeText x
 
newtype AffaireReference = AffaireReference Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireReference Xsd.XsdString where
    restricts (AffaireReference x) = x
instance SchemaType AffaireReference where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e  parseSimpleType
    schemaTypeToXML s (AffaireReference x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireReference where
    acceptingParser = fmap AffaireReference acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireReference x) = simpleTypeText x
 
data DonneesGenerales = DonneesGenerales
        { donneesGenerales_initiateurLogin :: InitiateurLogin
        , donneesGenerales_contratId :: ContratId
        , donneesGenerales_referenceDemandeur :: Maybe ReferenceDemandeur
        , donneesGenerales_affaireReference :: Maybe AffaireReference
        , donneesGenerales_referenceRegroupement :: Maybe ReferenceRegroupement
        }
        deriving (Eq,Show)
instance SchemaType DonneesGenerales where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DonneesGenerales
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "contratId"
            `apply` optional (parseSchemaType "referenceDemandeur")
            `apply` optional (parseSchemaType "affaireReference")
            `apply` optional (parseSchemaType "referenceRegroupement")
    schemaTypeToXML s x@DonneesGenerales{} =
        toXMLElement s []
            [ schemaTypeToXML "initiateurLogin" $ donneesGenerales_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGenerales_contratId x
            , maybe [] (schemaTypeToXML "referenceDemandeur") $ donneesGenerales_referenceDemandeur x
            , maybe [] (schemaTypeToXML "affaireReference") $ donneesGenerales_affaireReference x
            , maybe [] (schemaTypeToXML "referenceRegroupement") $ donneesGenerales_referenceRegroupement x
            ]
 
data Format
    = FormatJSON
    | FormatCSV
    deriving (Eq,Show,Enum)
instance SchemaType Format where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Format where
    acceptingParser =  do _ <- literal "JSON"; return FormatJSON
                      `onFail` do _ <- literal "CSV"; return FormatCSV
                      
    simpleTypeText FormatJSON = "JSON"
    simpleTypeText FormatCSV = "CSV"
 
newtype PointId = PointId Xsd.XsdString deriving (Eq,Show)
instance Restricts PointId Xsd.XsdString where
    restricts (PointId x) = x
instance SchemaType PointId where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PointId x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PointId where
    acceptingParser = fmap PointId acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{14})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (PointId x) = simpleTypeText x
 
data MesuresTypeCode
    = MesuresTypeCodeCOURBES
    | MesuresTypeCodeENERGIE
    | MesuresTypeCodePMAX
    | MesuresTypeCodeINDEX
    deriving (Eq,Show,Enum)
instance SchemaType MesuresTypeCode where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesuresTypeCode where
    acceptingParser =  do _ <- literal "COURBES"; return MesuresTypeCodeCOURBES
                      `onFail` do _ <- literal "ENERGIE"; return MesuresTypeCodeENERGIE
                      `onFail` do _ <- literal "PMAX"; return MesuresTypeCodePMAX
                      `onFail` do _ <- literal "INDEX"; return MesuresTypeCodeINDEX
                      
    simpleTypeText :: MesuresTypeCode -> String
    simpleTypeText MesuresTypeCodeCOURBES = "COURBES"
    simpleTypeText MesuresTypeCodeENERGIE = "ENERGIE"
    simpleTypeText MesuresTypeCodePMAX = "PMAX"
    simpleTypeText MesuresTypeCodeINDEX = "INDEX"
 
newtype MesuresCorrigees = MesuresCorrigees Xsd.Boolean deriving (Eq,Show)
instance Restricts MesuresCorrigees Xsd.Boolean where
    restricts (MesuresCorrigees x) = x
instance SchemaType MesuresCorrigees where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesuresCorrigees x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesuresCorrigees where
    acceptingParser = fmap MesuresCorrigees acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (MesuresCorrigees x) = simpleTypeText x
 
newtype DateDebut = DateDebut Xsd.Date deriving (Eq,Show)
instance Restricts DateDebut Xsd.Date where
    restricts (DateDebut x) = x
instance SchemaType DateDebut where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DateDebut x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DateDebut where
    acceptingParser = fmap DateDebut acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (DateDebut x) = simpleTypeText x
 
newtype DateFin = DateFin Xsd.Date deriving (Eq,Show)
instance Restricts DateFin Xsd.Date where
    restricts (DateFin x) = x
instance SchemaType DateFin where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DateFin x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DateFin where
    acceptingParser = fmap DateFin acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (DateFin x) = simpleTypeText x
 
data Sens
    = SensSOUTIRAGE
    | SensINJECTION
    deriving (Eq,Show,Enum)
instance SchemaType Sens where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Sens where
    acceptingParser =  do _ <- literal "SOUTIRAGE"; return SensSOUTIRAGE
                      `onFail` do _ <- literal "INJECTION"; return SensINJECTION
                      
    simpleTypeText SensSOUTIRAGE = "SOUTIRAGE"
    simpleTypeText SensINJECTION = "INJECTION"
 
data CadreAcces
    = CadreAccesSERVICEACCES
    | CadreAccesACCORDCLIENT
    deriving (Eq,Show,Enum)
instance SchemaType CadreAcces where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CadreAcces where
    acceptingParser =  do _ <- literal "SERVICE_ACCES"; return CadreAccesSERVICEACCES
                      `onFail` do _ <- literal "ACCORD_CLIENT"; return CadreAccesACCORDCLIENT
                      
    simpleTypeText CadreAccesSERVICEACCES = "SERVICE_ACCES"
    simpleTypeText CadreAccesACCORDCLIENT = "ACCORD_CLIENT"
 
newtype PointIds = PointIds
        { pointIds_pointId :: [PointId]
        }
        deriving (Eq,Show)
instance SchemaType PointIds where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PointIds
            `apply` between (Occurs (Just 1) (Just 10000))
                            (parseSchemaType "pointId")
    schemaTypeToXML s x@PointIds{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "pointId") $ pointIds_pointId x
            ]
 
data Demande = Demande
        { demande_format :: Maybe Format
        , demande_pointIds :: PointIds
        , demande_mesuresTypeCode :: MesuresTypeCode
        , demande_mesuresCorrigees :: Maybe MesuresCorrigees
        , demande_dateDebut :: DateDebut
        , demande_dateFin :: DateFin
        , demande_sens :: Sens
        , demande_cadreAcces :: CadreAcces
        }
        deriving (Eq,Show)
instance SchemaType Demande where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return Demande
            `apply` optional (parseSchemaType "format")
            `apply` parseSchemaType "pointIds"
            `apply` parseSchemaType "mesuresTypeCode"
            `apply` optional (parseSchemaType "mesuresCorrigees")
            `apply` parseSchemaType "dateDebut"
            `apply` parseSchemaType "dateFin"
            `apply` parseSchemaType "sens"
            `apply` parseSchemaType "cadreAcces"
    schemaTypeToXML s x@Demande{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "format") $ demande_format x
            , schemaTypeToXML "pointIds" $ demande_pointIds x
            , schemaTypeToXML "mesuresTypeCode" $ demande_mesuresTypeCode x
            , maybe [] (schemaTypeToXML "mesuresCorrigees") $ demande_mesuresCorrigees x
            , schemaTypeToXML "dateDebut" $ demande_dateDebut x
            , schemaTypeToXML "dateFin" $ demande_dateFin x
            , schemaTypeToXML "sens" $ demande_sens x
            , schemaTypeToXML "cadreAcces" $ demande_cadreAcces x
            ]
