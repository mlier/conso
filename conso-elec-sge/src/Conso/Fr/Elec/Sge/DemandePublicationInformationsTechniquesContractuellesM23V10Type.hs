{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type
  ( module Conso.Fr.Elec.Sge.DemandePublicationInformationsTechniquesContractuellesM23V10Type
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
 
elementDemandePublicationITC :: XMLParser DemandePublicationITC
elementDemandePublicationITC = parseSchemaType "sc:demandePublicationITC"
elementToXMLDemandePublicationITC :: DemandePublicationITC -> [Content ()]
elementToXMLDemandePublicationITC = schemaTypeToXML "sc:demandePublicationITC"
 
newtype AffaireId = AffaireId Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireId Xsd.XsdString where
    restricts (AffaireId x) = x
instance SchemaType AffaireId where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireId x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireId where
    acceptingParser = fmap AffaireId acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireId x) = simpleTypeText x
 
data DemandePublicationITC = DemandePublicationITC
        { demandePublicationITC_donneesGenerales :: DonneesGenerales
        , demandePublicationITC_demande :: Demande
        }
        deriving (Eq,Show)
instance SchemaType DemandePublicationITC where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandePublicationITC
            `apply` parseSchemaType "donneesGenerales"
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@DemandePublicationITC{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "https://sge-b2b.enedis.fr/services/commandeinformationstechniquesetcontractuelles/v1" 
                       ]
            [ schemaTypeToXML "donneesGenerales" $ demandePublicationITC_donneesGenerales x
            , schemaTypeToXML "demande" $ demandePublicationITC_demande x
            ]
 
newtype InitiateurLogin = InitiateurLogin Xsd.XsdString deriving (Eq,Show)
instance Restricts InitiateurLogin Xsd.XsdString where
    restricts (InitiateurLogin x) = x
instance SchemaType InitiateurLogin where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
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
        commit $ interior e parseSimpleType
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
        commit $ interior e parseSimpleType
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
        commit $ interior e parseSimpleType
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
        commit $ interior e parseSimpleType
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
    = Format_JSON
    | Format_CSV
    deriving (Eq,Show,Enum)
instance SchemaType Format where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Format where
    acceptingParser =  do _ <- literal "JSON"; return Format_JSON
                      `onFail` do _ <- literal "CSV"; return Format_CSV
                      
    simpleTypeText Format_JSON = "JSON"
    simpleTypeText Format_CSV = "CSV"
 
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
 
data Sens
    = Sens_SOUTIRAGE
    | Sens_INJECTION
    deriving (Eq,Show,Enum)
instance SchemaType Sens where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Sens where
    acceptingParser =  do _ <- literal "SOUTIRAGE"; return Sens_SOUTIRAGE
                      `onFail` do _ <- literal "INJECTION"; return Sens_INJECTION
                      
    simpleTypeText Sens_SOUTIRAGE = "SOUTIRAGE"
    simpleTypeText Sens_INJECTION = "INJECTION"
 
data CadreAcces
    = CadreAcces_SERVICE_ACCES
    | CadreAcces_EST_TITULAIRE
    | CadreAcces_ACCORD_CLIENT
    deriving (Eq,Show,Enum)
instance SchemaType CadreAcces where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CadreAcces where
    acceptingParser =  do _ <- literal "SERVICE_ACCES"; return CadreAcces_SERVICE_ACCES
                      `onFail` do _ <- literal "EST_TITULAIRE"; return CadreAcces_EST_TITULAIRE
                      `onFail` do _ <- literal "ACCORD_CLIENT"; return CadreAcces_ACCORD_CLIENT
                      
    simpleTypeText CadreAcces_SERVICE_ACCES = "SERVICE_ACCES"
    simpleTypeText CadreAcces_EST_TITULAIRE = "EST_TITULAIRE"
    simpleTypeText CadreAcces_ACCORD_CLIENT = "ACCORD_CLIENT"
 
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
            `apply` parseSchemaType "sens"
            `apply` parseSchemaType "cadreAcces"
    schemaTypeToXML s x@Demande{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "format") $ demande_format x
            , schemaTypeToXML "pointIds" $ demande_pointIds x
            , schemaTypeToXML "sens" $ demande_sens x
            , schemaTypeToXML "cadreAcces" $ demande_cadreAcces x
            ]
