{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
  ( module Conso.Fr.Elec.Sge.CommanderAccesDonneesMesuresV10Type
  ) where

import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

-- Some hs-boot imports are required, for fwd-declaring types.

elementCommanderAccesDonneesMesures :: XMLParser CommanderAccesDonneesMesuresType
elementCommanderAccesDonneesMesures = parseSchemaType "sc:commanderAccesDonneesMesures"
elementToXMLCommanderAccesDonneesMesures :: CommanderAccesDonneesMesuresType -> [Content ()]
elementToXMLCommanderAccesDonneesMesures = schemaTypeToXML "sc:commanderAccesDonneesMesures"

elementCommanderAccesDonneesMesuresResponse :: XMLParser CommanderAccesDonneesMesuresResponseType
elementCommanderAccesDonneesMesuresResponse = parseSchemaType "commanderAccesDonneesMesuresResponse"
elementToXMLCommanderAccesDonneesMesuresResponse :: CommanderAccesDonneesMesuresResponseType -> [Content ()]
elementToXMLCommanderAccesDonneesMesuresResponse = schemaTypeToXML "commanderAccesDonneesMesuresResponse"

newtype CommanderAccesDonneesMesuresType = CommanderAccesDonneesMesuresType
        { commanderAccesDonneesMesuresType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderAccesDonneesMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderAccesDonneesMesuresType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderAccesDonneesMesuresType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/commanderaccesdonneesmesures/v1.0" 
                       ]
            [ schemaTypeToXML "demande" $ commanderAccesDonneesMesuresType_demande x
            ]

data CommanderAccesDonneesMesuresResponseType = CommanderAccesDonneesMesuresResponseType
        { commanderAccesDonneesMesuresResponseType_affaireId :: AffaireIdType
        , commanderAccesDonneesMesuresResponseType_prestations :: Maybe PrestationsType
        , commanderAccesDonneesMesuresResponseType_serviceSouscritId :: Maybe ServiceSouscritIdType
        }
        deriving (Eq,Show)
instance SchemaType CommanderAccesDonneesMesuresResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderAccesDonneesMesuresResponseType
            `apply` parseSchemaType "affaireId"
            `apply` optional (parseSchemaType "prestations")
            `apply` optional (parseSchemaType "serviceSouscritId")
    schemaTypeToXML s x@CommanderAccesDonneesMesuresResponseType{} =
        toXMLElement s []
            [ schemaTypeToXML "affaireId" $ commanderAccesDonneesMesuresResponseType_affaireId x
            , maybe [] (schemaTypeToXML "prestations") $ commanderAccesDonneesMesuresResponseType_prestations x
            , maybe [] (schemaTypeToXML "serviceSouscritId") $ commanderAccesDonneesMesuresResponseType_serviceSouscritId x
            ]

data PrestationFicheType = PrestationFicheType
        { prestationFicheType_code :: PrestationFicheCodeType
        , prestationFicheType_libelle :: Maybe Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationFicheType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationFicheType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationFicheType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationFicheType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationFicheType_libelle x
            ]

data PrestationOptionType = PrestationOptionType
        { prestationOptionType_code :: PrestationOptionCodeType
        , prestationOptionType_libelle :: Maybe Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationOptionType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationOptionType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationOptionType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationOptionType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationOptionType_libelle x
            ]

data PrestationCasType = PrestationCasType
        { prestationCasType_code :: PrestationCasCodeType
        , prestationCasType_libelle :: Maybe Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationCasType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationCasType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationCasType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationCasType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationCasType_libelle x
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
        , prestationType_cas :: Maybe PrestationCasType
        }
        deriving (Eq,Show)
instance SchemaType PrestationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PrestationType
            `apply` parseSchemaType "rang"
            `apply` parseSchemaType "fiche"
            `apply` optional (parseSchemaType "option")
            `apply` optional (parseSchemaType "cas")
    schemaTypeToXML s x@PrestationType{} =
        toXMLElement s []
            [ schemaTypeToXML "rang" $ prestationType_rang x
            , schemaTypeToXML "fiche" $ prestationType_fiche x
            , maybe [] (schemaTypeToXML "option") $ prestationType_option x
            , maybe [] (schemaTypeToXML "cas") $ prestationType_cas x
            ]

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

newtype PrestationCasCodeType = PrestationCasCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PrestationCasCodeType Xsd.XsdString where
    restricts (PrestationCasCodeType x) = x
instance SchemaType PrestationCasCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PrestationCasCodeType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrestationCasCodeType where
    acceptingParser = fmap PrestationCasCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PrestationCasCodeType x) = simpleTypeText x

newtype ServiceSouscritIdType = ServiceSouscritIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts ServiceSouscritIdType Xsd.XsdString where
    restricts (ServiceSouscritIdType x) = x
instance SchemaType ServiceSouscritIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ServiceSouscritIdType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ServiceSouscritIdType where
    acceptingParser = fmap ServiceSouscritIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ServiceSouscritIdType x) = simpleTypeText x

data DemandeType = DemandeType
        { demandeType_donneesGenerales :: DonneesGeneralesType
        , demandeType_accesDonnees :: AccesDonneesType
        }
        deriving (Eq,Show)
instance SchemaType DemandeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandeType
            `apply` parseSchemaType "donneesGenerales"
            `apply` parseSchemaType "accesDonnees"
    schemaTypeToXML s x@DemandeType{} =
        toXMLElement s []
            [ schemaTypeToXML "donneesGenerales" $ demandeType_donneesGenerales x
            , schemaTypeToXML "accesDonnees" $ demandeType_accesDonnees x
            ]

data DonneesGeneralesType = DonneesGeneralesType
        { donneesGeneralesType_refExterne :: Maybe Chaine255Type
        , donneesGeneralesType_objetCode :: DemandeObjetCodeType
        , donneesGeneralesType_pointId :: PointIdType
        , donneesGeneralesType_initiateurLogin :: AdresseEmailType
        , donneesGeneralesType_contrat :: ContratType
        }
        deriving (Eq,Show)
instance SchemaType DonneesGeneralesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DonneesGeneralesType
            `apply` optional (parseSchemaType "refExterne")
            `apply` parseSchemaType "objetCode"
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "contrat"
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "refExterne") $ donneesGeneralesType_refExterne x
            , schemaTypeToXML "objetCode" $ donneesGeneralesType_objetCode x
            , schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contrat" $ donneesGeneralesType_contrat x
            ]

data DeclarationAccordClientType = DeclarationAccordClientType
        { declarationAccordClientType_accord :: BooleenType
        , declarationAccordClientType_choice1 :: Maybe (OneOf2 PersonnePhysiqueType PersonneMoraleType)
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

data AccesDonneesType = AccesDonneesType
        { accesDonneesType_dateDebut :: DateType
        , accesDonneesType_dateFin :: Maybe DateType
        , accesDonneesType_declarationAccordClient :: DeclarationAccordClientType
        , accesDonneesType_typeDonnees :: TypeDonneesType
        , accesDonneesType_soutirage :: Maybe BooleenType
        , accesDonneesType_injection :: Maybe BooleenType
        }
        deriving (Eq,Show)
instance SchemaType AccesDonneesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AccesDonneesType
            `apply` parseSchemaType "dateDebut"
            `apply` optional (parseSchemaType "dateFin")
            `apply` parseSchemaType "declarationAccordClient"
            `apply` parseSchemaType "typeDonnees"
            `apply` optional (parseSchemaType "soutirage")
            `apply` optional (parseSchemaType "injection")
    schemaTypeToXML s x@AccesDonneesType{} =
        toXMLElement s []
            [ schemaTypeToXML "dateDebut" $ accesDonneesType_dateDebut x
            , maybe [] (schemaTypeToXML "dateFin") $ accesDonneesType_dateFin x
            , schemaTypeToXML "declarationAccordClient" $ accesDonneesType_declarationAccordClient x
            , schemaTypeToXML "typeDonnees" $ accesDonneesType_typeDonnees x
            , maybe [] (schemaTypeToXML "soutirage") $ accesDonneesType_soutirage x
            , maybe [] (schemaTypeToXML "injection") $ accesDonneesType_injection x
            ]

data ContratType = ContratType
        { contratType_contratId :: Maybe ContratIdType
        , contratType_acteurMarcheCode :: Maybe ActeurCodeType
        , contratType_contratType :: Maybe ContratTypeType
        }
        deriving (Eq,Show)
instance SchemaType ContratType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ContratType
            `apply` optional (parseSchemaType "contratId")
            `apply` optional (parseSchemaType "acteurMarcheCode")
            `apply` optional (parseSchemaType "contratType")
    schemaTypeToXML s x@ContratType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "contratId") $ contratType_contratId x
            , maybe [] (schemaTypeToXML "acteurMarcheCode") $ contratType_acteurMarcheCode x
            , maybe [] (schemaTypeToXML "contratType") $ contratType_contratType x
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

newtype ContratTypeType = ContratTypeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ContratTypeType Xsd.XsdString where
    restricts (ContratTypeType x) = x
instance SchemaType ContratTypeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ContratTypeType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ContratTypeType where
    acceptingParser = fmap ContratTypeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ContratTypeType x) = simpleTypeText x

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

newtype DemandeObjetCodeType = DemandeObjetCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeObjetCodeType Xsd.XsdString where
    restricts (DemandeObjetCodeType x) = x
instance SchemaType DemandeObjetCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeObjetCodeType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeObjetCodeType where
    acceptingParser = fmap DemandeObjetCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeObjetCodeType x) = simpleTypeText x

data CiviliteAbreviationType
    = CiviliteAbreviationTypeM
    | CiviliteAbreviationTypeMme
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

newtype FinaliteCodeType = FinaliteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FinaliteCodeType Xsd.XsdString where
    restricts (FinaliteCodeType x) = x
instance SchemaType FinaliteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FinaliteCodeType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FinaliteCodeType where
    acceptingParser = fmap FinaliteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FinaliteCodeType x) = simpleTypeText x

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

newtype PeriodiciteTransmissionCodeType = PeriodiciteTransmissionCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PeriodiciteTransmissionCodeType Xsd.XsdString where
    restricts (PeriodiciteTransmissionCodeType x) = x
instance SchemaType PeriodiciteTransmissionCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PeriodiciteTransmissionCodeType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PeriodiciteTransmissionCodeType where
    acceptingParser = fmap PeriodiciteTransmissionCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PeriodiciteTransmissionCodeType x) = simpleTypeText x

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

newtype ActeurCodeType = ActeurCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ActeurCodeType Xsd.XsdString where
    restricts (ActeurCodeType x) = x
instance SchemaType ActeurCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ActeurCodeType x) =
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ActeurCodeType where
    acceptingParser = fmap ActeurCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ActeurCodeType x) = simpleTypeText x

--data UtilisateurLoginType = UtilisateurLoginType
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
