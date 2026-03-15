{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
  ( module Conso.Fr.Elec.Sge.CommanderRenouvellementServicesAccesDonneesV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderRenouvellementServicesAccesDonnees :: XMLParser RenouvelerServicesAccesType
elementCommanderRenouvellementServicesAccesDonnees = parseSchemaType "commanderRenouvellementServicesAccesDonnees"
elementToXMLCommanderRenouvellementServicesAccesDonnees :: RenouvelerServicesAccesType -> [Content ()]
elementToXMLCommanderRenouvellementServicesAccesDonnees = schemaTypeToXML "commanderRenouvellementServicesAccesDonnees"
 
elementCommanderRenouvellementServicesAccesDonneesResponse :: XMLParser RenouvelerServicesAccesResponseType
elementCommanderRenouvellementServicesAccesDonneesResponse = parseSchemaType "commanderRenouvellementServicesAccesDonneesResponse"
elementToXMLCommanderRenouvellementServicesAccesDonneesResponse :: RenouvelerServicesAccesResponseType -> [Content ()]
elementToXMLCommanderRenouvellementServicesAccesDonneesResponse = schemaTypeToXML "commanderRenouvellementServicesAccesDonneesResponse"
 
newtype RenouvelerServicesAccesType = RenouvelerServicesAccesType
        { renouvelerServicesAccesType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType RenouvelerServicesAccesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RenouvelerServicesAccesType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@RenouvelerServicesAccesType{} =
        toXMLElement s []
            [ schemaTypeToXML "demande" $ renouvelerServicesAccesType_demande x
            ]
 
newtype RenouvelerServicesAccesResponseType = RenouvelerServicesAccesResponseType
        { renouvelerServicesAccesResponseType_servicesRenouveles :: Maybe ServicesRenouvelesType
        }
        deriving (Eq,Show)
instance SchemaType RenouvelerServicesAccesResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RenouvelerServicesAccesResponseType
            `apply` optional (parseSchemaType "servicesRenouveles")
    schemaTypeToXML s x@RenouvelerServicesAccesResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "servicesRenouveles") $ renouvelerServicesAccesResponseType_servicesRenouveles x
            ]
 
newtype ServicesRenouvelesType = ServicesRenouvelesType
        { servicesRenouvelesType_serviceRenouvele :: [ServiceRenouveleType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesRenouvelesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesRenouvelesType
            `apply` many1 (parseSchemaType "serviceRenouvele")
    schemaTypeToXML s x@ServicesRenouvelesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceRenouvele") $ servicesRenouvelesType_serviceRenouvele x
            ]
 
data ServiceRenouveleType = ServiceRenouveleType
        { serviceRenouveleType_affaireId :: AffaireIdType
        , serviceRenouveleType_serviceSouscritId :: ServiceIdType
        }
        deriving (Eq,Show)
instance SchemaType ServiceRenouveleType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceRenouveleType
            `apply` parseSchemaType "affaireId"
            `apply` parseSchemaType "serviceSouscritId"
    schemaTypeToXML s x@ServiceRenouveleType{} =
        toXMLElement s []
            [ schemaTypeToXML "affaireId" $ serviceRenouveleType_affaireId x
            , schemaTypeToXML "serviceSouscritId" $ serviceRenouveleType_serviceSouscritId x
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
        , donneesGeneralesType_declarationAccordClient :: Maybe DeclarationAccordClientType
        , donneesGeneralesType_dateFin :: Maybe DateType
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
            `apply` optional (parseSchemaType "declarationAccordClient")
            `apply` optional (parseSchemaType "dateFin")
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGeneralesType_contratId x
            , schemaTypeToXML "sens" $ donneesGeneralesType_sens x
            , maybe [] (schemaTypeToXML "declarationAccordClient") $ donneesGeneralesType_declarationAccordClient x
            , maybe [] (schemaTypeToXML "dateFin") $ donneesGeneralesType_dateFin x
            ]
 
data DeclarationAccordClientType = DeclarationAccordClientType
        { declarationAccordClientType_accord :: BooleenType
        , declarationAccordClientType_choice1 :: Maybe (OneOf2 PersonnePhysiqueType PersonneMoraleType)
          -- ^ Choice between:
          --   
          --   (1) personnePhysique
          --   
          --   (2) personneMorale
        }
        deriving (Eq,Show)
instance SchemaType DeclarationAccordClientType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DeclarationAccordClientType
            `apply` parseSchemaType "accord"
            `apply` optional (oneOf' [ ("PersonnePhysiqueType", fmap OneOf2 (parseSchemaType "personnePhysique"))
                                     , ("PersonneMoraleType", fmap TwoOf2 (parseSchemaType "personneMorale"))
                                     ])
    schemaTypeToXML s x@DeclarationAccordClientType{} =
        toXMLElement s []
            [ schemaTypeToXML "accord" $ declarationAccordClientType_accord x
            , maybe [] (foldOneOf2  (schemaTypeToXML "personnePhysique")
                                    (schemaTypeToXML "personneMorale")
                                   ) $ declarationAccordClientType_choice1 x
            ]
 
newtype ServicesSouscritsType = ServicesSouscritsType
        { servicesSouscritsType_serviceSouscritId :: [ServiceIdType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesSouscritsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesSouscritsType
            `apply` many1 (parseSchemaType "serviceSouscritId")
    schemaTypeToXML s x@ServicesSouscritsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceSouscritId") $ servicesSouscritsType_serviceSouscritId x
            ]
 
newtype Chaine255Type = Chaine255Type Xsd.XsdString deriving (Eq,Show)
instance Restricts Chaine255Type Xsd.XsdString where
    restricts (Chaine255Type x) = x
instance SchemaType Chaine255Type where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (Chaine255Type x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Chaine255Type where
    acceptingParser = fmap Chaine255Type acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (Chaine255Type x) = simpleTypeText x
 
newtype AffaireIdType = AffaireIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireIdType Xsd.XsdString where
    restricts (AffaireIdType x) = x
instance SchemaType AffaireIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireIdType where
    acceptingParser = fmap AffaireIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9A-Z]{4,8})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (AffaireIdType x) = simpleTypeText x
 
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
 
newtype DateType = DateType Xsd.Date deriving (Eq,Show)
instance Restricts DateType Xsd.Date where
    restricts (DateType x) = x
instance SchemaType DateType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DateType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DateType where
    acceptingParser = fmap DateType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (DateType x) = simpleTypeText x
 
data CiviliteAbreviationType = CiviliteAbreviationTypeM | CiviliteAbreviationTypeMme
    deriving (Eq,Show,Enum)
instance SchemaType CiviliteAbreviationType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CiviliteAbreviationType where
    acceptingParser =  do _ <- literal "M"; return CiviliteAbreviationTypeM
                      `onFail` do _ <- literal "Mme"; return CiviliteAbreviationTypeMme
                      
    simpleTypeText CiviliteAbreviationTypeM = "M"
    simpleTypeText CiviliteAbreviationTypeMme = "Mme"
 
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
 
data PersonnePhysiqueType = PersonnePhysiqueType
        { personnePhysiqueType_civilite :: Maybe CiviliteAbreviationType
        , personnePhysiqueType_nom :: Chaine255Type
        , personnePhysiqueType_prenom :: Maybe Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PersonnePhysiqueType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PersonnePhysiqueType
            `apply` optional (parseSchemaType "civilite")
            `apply` parseSchemaType "nom"
            `apply` optional (parseSchemaType "prenom")
    schemaTypeToXML s x@PersonnePhysiqueType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "civilite") $ personnePhysiqueType_civilite x
            , schemaTypeToXML "nom" $ personnePhysiqueType_nom x
            , maybe [] (schemaTypeToXML "prenom") $ personnePhysiqueType_prenom x
            ]
 
newtype PersonneMoraleType = PersonneMoraleType
        { personneMoraleType_denominationSociale :: Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PersonneMoraleType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PersonneMoraleType
            `apply` parseSchemaType "denominationSociale"
    schemaTypeToXML s x@PersonneMoraleType{} =
        toXMLElement s []
            [ schemaTypeToXML "denominationSociale" $ personneMoraleType_denominationSociale x
            ]
