{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
  ( module Conso.Fr.Elec.Sge.CommanderServicesAccesDonneesV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderServicesAccesDonnees :: XMLParser CommanderServicesAccesDonneesType
elementCommanderServicesAccesDonnees = parseSchemaType "commanderServicesAccesDonnees"
elementToXMLCommanderServicesAccesDonnees :: CommanderServicesAccesDonneesType -> [Content ()]
elementToXMLCommanderServicesAccesDonnees = schemaTypeToXML "commanderServicesAccesDonnees"
 
elementCommanderServicesAccesDonneesResponse :: XMLParser CommanderServicesAccesDonneesResponseType
elementCommanderServicesAccesDonneesResponse = parseSchemaType "commanderServicesAccesDonneesResponse"
elementToXMLCommanderServicesAccesDonneesResponse :: CommanderServicesAccesDonneesResponseType -> [Content ()]
elementToXMLCommanderServicesAccesDonneesResponse = schemaTypeToXML "commanderServicesAccesDonneesResponse"
 
newtype CommanderServicesAccesDonneesType = CommanderServicesAccesDonneesType
        { commanderServicesAccesDonneesType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderServicesAccesDonneesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderServicesAccesDonneesType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderServicesAccesDonneesType{} =
        toXMLElement s []
            [ schemaTypeToXML "demande" $ commanderServicesAccesDonneesType_demande x
            ]
 
newtype CommanderServicesAccesDonneesResponseType = CommanderServicesAccesDonneesResponseType
        { commanderServicesAccesDonneesResponseType_affaires :: Maybe AffairesType
        }
        deriving (Eq,Show)
instance SchemaType CommanderServicesAccesDonneesResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderServicesAccesDonneesResponseType
            `apply` optional (parseSchemaType "affaires")
    schemaTypeToXML s x@CommanderServicesAccesDonneesResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "affaires") $ commanderServicesAccesDonneesResponseType_affaires x
            ]
 
newtype AffairesType = AffairesType
        { affairesType_affaire :: [AffaireType]
        }
        deriving (Eq,Show)
instance SchemaType AffairesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AffairesType
            `apply` between (Occurs (Just 1) (Just 100000))
                            (parseSchemaType "affaire")
    schemaTypeToXML s x@AffairesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "affaire") $ affairesType_affaire x
            ]
 
data AffaireType = AffaireType
        { affaireType_affaireId :: AffaireIdType
        , affaireType_prestations :: PrestationsType
        , affaireType_serviceSouscritMesures :: ServiceSouscritMesuresType
        }
        deriving (Eq,Show)
instance SchemaType AffaireType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AffaireType
            `apply` parseSchemaType "affaireId"
            `apply` parseSchemaType "prestations"
            `apply` parseSchemaType "serviceSouscritMesures"
    schemaTypeToXML s x@AffaireType{} =
        toXMLElement s []
            [ schemaTypeToXML "affaireId" $ affaireType_affaireId x
            , schemaTypeToXML "prestations" $ affaireType_prestations x
            , schemaTypeToXML "serviceSouscritMesures" $ affaireType_serviceSouscritMesures x
            ]
 
newtype PrestationsType = PrestationsType
        { prestationsType_prestation :: PrestationType
        }
        deriving (Eq,Show)
instance SchemaType PrestationsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PrestationsType
            `apply` parseSchemaType "prestation"
    schemaTypeToXML s x@PrestationsType{} =
        toXMLElement s []
            [ schemaTypeToXML "prestation" $ prestationsType_prestation x
            ]
 
data PrestationType = PrestationType
        { prestationType_rang :: NbEntierType
        , prestationType_fiche :: PrestationFicheType
        , prestationType_option :: Maybe PrestationOptionType
        }
        deriving (Eq,Show)
instance SchemaType PrestationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PrestationType
            `apply` parseSchemaType "rang"
            `apply` parseSchemaType "fiche"
            `apply` optional (parseSchemaType "option")
    schemaTypeToXML s x@PrestationType{} =
        toXMLElement s []
            [ schemaTypeToXML "rang" $ prestationType_rang x
            , schemaTypeToXML "fiche" $ prestationType_fiche x
            , maybe [] (schemaTypeToXML "option") $ prestationType_option x
            ]
 
data PrestationFicheType = PrestationFicheType
        { prestationFicheType_libelle :: Chaine255Type
        , prestationFicheType_code :: PrestationFicheCodeType
        }
        deriving (Eq,Show)
instance SchemaType PrestationFicheType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "libelle" e pos
        a1 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationFicheType a0 a1)
    schemaTypeToXML s x@PrestationFicheType{} =
        toXMLElement s [ toXMLAttribute "libelle" $ prestationFicheType_libelle x
                       , toXMLAttribute "code" $ prestationFicheType_code x
                       ]
            []
 
data PrestationOptionType = PrestationOptionType
        { prestationOptionType_libelle :: Chaine255Type
        , prestationOptionType_code :: PrestationOptionCodeType
        }
        deriving (Eq,Show)
instance SchemaType PrestationOptionType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "libelle" e pos
        a1 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationOptionType a0 a1)
    schemaTypeToXML s x@PrestationOptionType{} =
        toXMLElement s [ toXMLAttribute "libelle" $ prestationOptionType_libelle x
                       , toXMLAttribute "code" $ prestationOptionType_code x
                       ]
            []
 
newtype PrestationFicheCodeType = PrestationFicheCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PrestationFicheCodeType Xsd.XsdString where
    restricts (PrestationFicheCodeType x) = x
instance SchemaType PrestationFicheCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PrestationFicheCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrestationFicheCodeType where
    acceptingParser = fmap PrestationFicheCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PrestationFicheCodeType x) = simpleTypeText x
 
newtype PrestationOptionCodeType = PrestationOptionCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PrestationOptionCodeType Xsd.XsdString where
    restricts (PrestationOptionCodeType x) = x
instance SchemaType PrestationOptionCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PrestationOptionCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrestationOptionCodeType where
    acceptingParser = fmap PrestationOptionCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PrestationOptionCodeType x) = simpleTypeText x
 
data ServiceSouscritMesuresType = ServiceSouscritMesuresType
        { serviceSouscritMesuresType_serviceSouscritId :: ServiceIdType
        , serviceSouscritMesuresType_mesuresTypeCode :: TypeDonneesType
        }
        deriving (Eq,Show)
instance SchemaType ServiceSouscritMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceSouscritMesuresType
            `apply` parseSchemaType "serviceSouscritId"
            `apply` parseSchemaType "mesuresTypeCode"
    schemaTypeToXML s x@ServiceSouscritMesuresType{} =
        toXMLElement s []
            [ schemaTypeToXML "serviceSouscritId" $ serviceSouscritMesuresType_serviceSouscritId x
            , schemaTypeToXML "mesuresTypeCode" $ serviceSouscritMesuresType_mesuresTypeCode x
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
        { donneesGeneralesType_refExterne :: Maybe Chaine255Type
        , donneesGeneralesType_pointId :: PointIdType
        , donneesGeneralesType_initiateurLogin :: UtilisateurLoginType
        , donneesGeneralesType_contratId :: ContratIdType
        , donneesGeneralesType_dateFin :: Maybe DateType
        , donneesGeneralesType_sens :: SensType
        , donneesGeneralesType_declarationAccordClient :: Maybe DeclarationAccordClientType
        }
        deriving (Eq,Show)
instance SchemaType DonneesGeneralesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DonneesGeneralesType
            `apply` optional (parseSchemaType "refExterne")
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "contratId"
            `apply` optional (parseSchemaType "dateFin")
            `apply` parseSchemaType "sens"
            `apply` optional (parseSchemaType "declarationAccordClient")
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "refExterne") $ donneesGeneralesType_refExterne x
            , schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGeneralesType_contratId x
            , maybe [] (schemaTypeToXML "dateFin") $ donneesGeneralesType_dateFin x
            , schemaTypeToXML "sens" $ donneesGeneralesType_sens x
            , maybe [] (schemaTypeToXML "declarationAccordClient") $ donneesGeneralesType_declarationAccordClient x
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
        { serviceSouscritType_typeDonnees :: TypeDonneesType
        , serviceSouscritType_optionsPublication :: Maybe OptionsPublicationType
        }
        deriving (Eq,Show)
instance SchemaType ServiceSouscritType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceSouscritType
            `apply` parseSchemaType "typeDonnees"
            `apply` optional (parseSchemaType "optionsPublication")
    schemaTypeToXML s x@ServiceSouscritType{} =
        toXMLElement s []
            [ schemaTypeToXML "typeDonnees" $ serviceSouscritType_typeDonnees x
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
 
newtype TypeDonneesType = TypeDonneesType Xsd.XsdString deriving (Eq,Show)
instance Restricts TypeDonneesType Xsd.XsdString where
    restricts (TypeDonneesType x) = x
instance SchemaType TypeDonneesType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TypeDonneesType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TypeDonneesType where
    acceptingParser = fmap TypeDonneesType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (TypeDonneesType x) = simpleTypeText x
 
newtype NbEntierType = NbEntierType Xsd.Integer deriving (Eq,Show)
instance Restricts NbEntierType Xsd.Integer where
    restricts (NbEntierType x) = x
instance SchemaType NbEntierType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (NbEntierType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NbEntierType where
    acceptingParser = fmap NbEntierType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (NbEntierType x) = simpleTypeText x
 
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
