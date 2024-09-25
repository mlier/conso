{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
  ( module Conso.Fr.Elec.Sge.ConsulterMesuresDetailleesCommunV12Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( Content,
      XMLParser,
      Extension(..),
      SchemaType(..),
      SimpleType(..),
      optional,
      literal,
      apply,
      many1,
      onFail,
      Alternative(many),
      Commitment(commit),
      parseSimpleType,
      toXMLAttribute,
      toXMLElement,
      toXMLText,
      element,
      interior,
      posnElement )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementConsulterMesuresDetailleesV3 :: XMLParser ConsulterMesuresDetailleesV3Type
elementConsulterMesuresDetailleesV3 = parseSchemaType "sc:consulterMesuresDetailleesV3"
elementToXMLConsulterMesuresDetailleesV3 :: ConsulterMesuresDetailleesV3Type -> [Content ()]
elementToXMLConsulterMesuresDetailleesV3 = schemaTypeToXML "sc:consulterMesuresDetailleesV3"
 
elementConsulterMesuresDetailleesResponseV3 :: XMLParser ConsulterMesuresDetailleesV3ResponseType
elementConsulterMesuresDetailleesResponseV3 = parseSchemaType "ns4:consulterMesuresDetailleesResponseV3"
elementToXMLConsulterMesuresDetailleesResponseV3 :: ConsulterMesuresDetailleesV3ResponseType -> [Content ()]
elementToXMLConsulterMesuresDetailleesResponseV3 = schemaTypeToXML "ns4:consulterMesuresDetailleesResponseV3"
 
newtype ConsulterMesuresDetailleesV3Type = ConsulterMesuresDetailleesV3Type
        { consulterMesuresDetailleesV3Type_demande :: Demande
        }
        deriving (Eq,Show)
instance SchemaType ConsulterMesuresDetailleesV3Type where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ConsulterMesuresDetailleesV3Type
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@ConsulterMesuresDetailleesV3Type{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ 
                        Xsd.XsdString "http://www.enedis.fr/sge/b2b/services/consultationmesuresdetaillees/common" 
                       ]
            [ schemaTypeToXML "demande" $ consulterMesuresDetailleesV3Type_demande x
            ]
 
data Demande = Demande
        { demande_initiateurLogin :: Xsd.XsdString
        , demande_pointId :: PointIdType
        , demande_mesuresTypeCode :: MesuresTypeCodeType
        , demande_grandeurPhysique :: Xsd.XsdString
        , demande_dateDebut :: Xsd.Date
        , demande_dateFin :: Xsd.Date
        , demande_mesuresPas :: Maybe MesuresPasType
        , demande_mesuresCorrigees :: Xsd.Boolean
        , demande_sens :: SensMesureType
        , demande_cadreAcces :: CadreAccesType
        }
        deriving (Eq,Show)
instance SchemaType Demande where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return Demande
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "mesuresTypeCode"
            `apply` parseSchemaType "grandeurPhysique"
            `apply` parseSchemaType "dateDebut"
            `apply` parseSchemaType "dateFin"
            `apply` optional (parseSchemaType "mesuresPas")
            `apply` parseSchemaType "mesuresCorrigees"
            `apply` parseSchemaType "sens"
            `apply` parseSchemaType "cadreAcces"
    schemaTypeToXML s x@Demande{} =
        toXMLElement s []
            [ schemaTypeToXML "initiateurLogin" $ demande_initiateurLogin x
            , schemaTypeToXML "pointId" $ demande_pointId x
            , schemaTypeToXML "mesuresTypeCode" $ demande_mesuresTypeCode x
            , schemaTypeToXML "grandeurPhysique" $ demande_grandeurPhysique x
            , schemaTypeToXML "dateDebut" $ demande_dateDebut x
            , schemaTypeToXML "dateFin" $ demande_dateFin x
            , maybe [] (schemaTypeToXML "mesuresPas") $ demande_mesuresPas x
            , schemaTypeToXML "mesuresCorrigees" $ demande_mesuresCorrigees x
            , schemaTypeToXML "sens" $ demande_sens x
            , schemaTypeToXML "cadreAcces" $ demande_cadreAcces x
            ]
 
--newtype PointIdType = PointIdType Xsd.XsdString deriving (Eq,Show)
--instance Restricts PointIdType Xsd.XsdString where
--    restricts (PointIdType x) = x
--instance SchemaType PointIdType where
--    parseSchemaType s = do
--        e <- element [s]
--        commit $ interior e $ parseSimpleType
--    schemaTypeToXML s (PointIdType x) = 
--        toXMLElement s [] [toXMLText (simpleTypeText x)]
--instance SimpleType PointIdType where
--    acceptingParser = fmap PointIdType acceptingParser
--    -- XXX should enforce the restrictions somehow?
--    -- The restrictions are:
--    --      (RangeR (Occurs Nothing Nothing))
--    --      (Pattern [0-9]{14})
--    --      (Enumeration)
--    --      (StrLength (Occurs Nothing Nothing))
--    simpleTypeText (PointIdType x) = simpleTypeText x
 
data MesuresTypeCodeType
    = MesuresTypeCodeTypeCOURBE
    | MesuresTypeCodeTypePMAX
    | MesuresTypeCodeTypeENERGIE
    | MesuresTypeCodeTypeINDEX
    deriving (Eq,Show,Enum)
instance SchemaType MesuresTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesuresTypeCodeType where
    acceptingParser =  do _ <- literal "COURBE"; return MesuresTypeCodeTypeCOURBE
                      `onFail` do _ <- literal "PMAX"; return MesuresTypeCodeTypePMAX
                      `onFail` do _ <- literal "ENERGIE"; return MesuresTypeCodeTypeENERGIE
                      `onFail` do _ <- literal "INDEX"; return MesuresTypeCodeTypeINDEX
                      
    simpleTypeText MesuresTypeCodeTypeCOURBE = "COURBE"
    simpleTypeText MesuresTypeCodeTypePMAX = "PMAX"
    simpleTypeText MesuresTypeCodeTypeENERGIE = "ENERGIE"
    simpleTypeText MesuresTypeCodeTypeINDEX = "INDEX"
 
data MesuresPasType
    = MesuresPasType_P1D
    | MesuresPasType_P1M
    deriving (Eq,Show,Enum)
instance SchemaType MesuresPasType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesuresPasType where
    acceptingParser =  do _ <- literal "P1D"; return MesuresPasType_P1D
                      `onFail` do _ <- literal "P1M"; return MesuresPasType_P1M
                      
    simpleTypeText MesuresPasType_P1D = "P1D"
    simpleTypeText MesuresPasType_P1M = "P1M"
 
data SensMesureType
    = SensMesureTypeINJECTION
    | SensMesureTypeSOUTIRAGE
    deriving (Eq,Show,Enum)
instance SchemaType SensMesureType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType SensMesureType where
    acceptingParser =  do _ <- literal "INJECTION"; return SensMesureTypeINJECTION
                      `onFail` do _ <- literal "SOUTIRAGE"; return SensMesureTypeSOUTIRAGE
                      
    simpleTypeText SensMesureTypeINJECTION = "INJECTION"
    simpleTypeText SensMesureTypeSOUTIRAGE = "SOUTIRAGE"
 
data CadreAccesType
    = CadreAccesTypeACCORDCLIENT
    | CadreAccesTypeSERVICEACCES
    | CadreAccesTypeESTTITULAIRE
    deriving (Eq,Show,Enum)
instance SchemaType CadreAccesType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CadreAccesType where
    acceptingParser =  do _ <- literal "ACCORD_CLIENT"; return CadreAccesTypeACCORDCLIENT
                      `onFail` do _ <- literal "SERVICE_ACCES"; return CadreAccesTypeSERVICEACCES
                      `onFail` do _ <- literal "EST_TITULAIRE"; return CadreAccesTypeESTTITULAIRE
                      
    simpleTypeText CadreAccesTypeACCORDCLIENT = "ACCORD_CLIENT"
    simpleTypeText CadreAccesTypeSERVICEACCES = "SERVICE_ACCES"
    simpleTypeText CadreAccesTypeESTTITULAIRE = "EST_TITULAIRE"
 
data ConsulterMesuresDetailleesV3ResponseType = ConsulterMesuresDetailleesV3ResponseType
        { consulterMesuresDetailleesV3ResponseType_pointId :: Maybe PointIdType
        , consulterMesuresDetailleesV3ResponseType_mesuresCorrigees :: Maybe Xsd.XsdString
        , consulterMesuresDetailleesV3ResponseType_periode :: Maybe PeriodeType
        , consulterMesuresDetailleesV3ResponseType_grandeur :: [GrandeurInstantanees]
        , consulterMesuresDetailleesV3ResponseType_contexte :: [Contexte]
        , consulterMesuresDetailleesV3ResponseType_typeValeur :: Maybe Xsd.XsdString
        , consulterMesuresDetailleesV3ResponseType_modeCalcul :: Maybe Xsd.XsdString
        , consulterMesuresDetailleesV3ResponseType_pas :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType ConsulterMesuresDetailleesV3ResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ConsulterMesuresDetailleesV3ResponseType
            `apply` optional (parseSchemaType "pointId")
            `apply` optional (parseSchemaType "mesuresCorrigees")
            `apply` optional (parseSchemaType "periode")
            `apply` many (parseSchemaType "grandeur")
            `apply` many (parseSchemaType "contexte")
            `apply` optional (parseSchemaType "typeValeur")
            `apply` optional (parseSchemaType "modeCalcul")
            `apply` optional (parseSchemaType "pas")
    schemaTypeToXML s x@ConsulterMesuresDetailleesV3ResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "pointId") $ consulterMesuresDetailleesV3ResponseType_pointId x
            , maybe [] (schemaTypeToXML "mesuresCorrigees") $ consulterMesuresDetailleesV3ResponseType_mesuresCorrigees x
            , maybe [] (schemaTypeToXML "periode") $ consulterMesuresDetailleesV3ResponseType_periode x
            , concatMap (schemaTypeToXML "grandeur") $ consulterMesuresDetailleesV3ResponseType_grandeur x
            , concatMap (schemaTypeToXML "contexte") $ consulterMesuresDetailleesV3ResponseType_contexte x
            , maybe [] (schemaTypeToXML "typeValeur") $ consulterMesuresDetailleesV3ResponseType_typeValeur x
            , maybe [] (schemaTypeToXML "modeCalcul") $ consulterMesuresDetailleesV3ResponseType_modeCalcul x
            , maybe [] (schemaTypeToXML "pas") $ consulterMesuresDetailleesV3ResponseType_pas x
            ]
 
data PeriodeType = PeriodeType
        { periodeType_dateDebut :: Xsd.Date
        , periodeType_dateFin :: Xsd.Date
        }
        deriving (Eq,Show)
instance SchemaType PeriodeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PeriodeType
            `apply` parseSchemaType "dateDebut"
            `apply` parseSchemaType "dateFin"
    schemaTypeToXML s x@PeriodeType{} =
        toXMLElement s []
            [ schemaTypeToXML "dateDebut" $ periodeType_dateDebut x
            , schemaTypeToXML "dateFin" $ periodeType_dateFin x
            ]
 
data GrandeurBase = GrandeurBase
        { grandeurBase_grandeurMetier :: Xsd.XsdString
        , grandeurBase_grandeurPhysique :: Xsd.XsdString
        , grandeurBase_unite :: Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType GrandeurBase where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return GrandeurBase
            `apply` parseSchemaType "grandeurMetier"
            `apply` parseSchemaType "grandeurPhysique"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@GrandeurBase{} =
        toXMLElement s []
            [ schemaTypeToXML "grandeurMetier" $ grandeurBase_grandeurMetier x
            , schemaTypeToXML "grandeurPhysique" $ grandeurBase_grandeurPhysique x
            , schemaTypeToXML "unite" $ grandeurBase_unite x
            ]
 
data GrandeurInstantanees = GrandeurInstantanees
        { grandeurInstantanees_grandeurMetier :: Xsd.XsdString
        , grandeurInstantanees_grandeurPhysique :: Xsd.XsdString
        , grandeurInstantanees_unite :: Xsd.XsdString
        , grandeurInstantanees_points :: [Points]
        }
        deriving (Eq,Show)
instance SchemaType GrandeurInstantanees where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return GrandeurInstantanees
            `apply` parseSchemaType "grandeurMetier"
            `apply` parseSchemaType "grandeurPhysique"
            `apply` parseSchemaType "unite"
            `apply` many1 (parseSchemaType "points")
    schemaTypeToXML s x@GrandeurInstantanees{} =
        toXMLElement s []
            [ schemaTypeToXML "grandeurMetier" $ grandeurInstantanees_grandeurMetier x
            , schemaTypeToXML "grandeurPhysique" $ grandeurInstantanees_grandeurPhysique x
            , schemaTypeToXML "unite" $ grandeurInstantanees_unite x
            , concatMap (schemaTypeToXML "points") $ grandeurInstantanees_points x
            ]
instance Extension GrandeurInstantanees GrandeurBase where
    supertype (GrandeurInstantanees e0 e1 e2 _) =
               GrandeurBase e0 e1 e2
 
data GrandeurType = GrandeurType
        { grandeurType_grandeurMetier :: Xsd.XsdString
        , grandeurType_grandeurPhysique :: Xsd.XsdString
        , grandeurType_unite :: Xsd.XsdString
        , grandeurType_calendrier :: [Calendrier]
        , grandeurType_cadranTotalisateur :: Maybe ClasseTemporelleTotalisateur
        }
        deriving (Eq,Show)
instance SchemaType GrandeurType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return GrandeurType
            `apply` parseSchemaType "grandeurMetier"
            `apply` parseSchemaType "grandeurPhysique"
            `apply` parseSchemaType "unite"
            `apply` many (parseSchemaType "calendrier")
            `apply` optional (parseSchemaType "cadranTotalisateur")
    schemaTypeToXML s x@GrandeurType{} =
        toXMLElement s []
            [ schemaTypeToXML "grandeurMetier" $ grandeurType_grandeurMetier x
            , schemaTypeToXML "grandeurPhysique" $ grandeurType_grandeurPhysique x
            , schemaTypeToXML "unite" $ grandeurType_unite x
            , concatMap (schemaTypeToXML "calendrier") $ grandeurType_calendrier x
            , maybe [] (schemaTypeToXML "cadranTotalisateur") $ grandeurType_cadranTotalisateur x
            ]
instance Extension GrandeurType GrandeurBase where
    supertype (GrandeurType e0 e1 e2 _ _) =
               GrandeurBase e0 e1 e2
 
data Points = Points
        { points_v :: Xsd.XsdString
        , points_d :: Xsd.XsdString
        , points_p :: Maybe Xsd.XsdString
        , points_n :: Maybe Xsd.XsdString
        , points_tc :: Maybe Xsd.XsdString
        , points_iv :: Maybe Xsd.XsdString
        , points_ec :: Maybe Xsd.XsdString
        }
        deriving (Eq,Show)
instance SchemaType Points where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return Points
            `apply` parseSchemaType "v"
            `apply` parseSchemaType "d"
            `apply` optional (parseSchemaType "p")
            `apply` optional (parseSchemaType "n")
            `apply` optional (parseSchemaType "tc")
            `apply` optional (parseSchemaType "iv")
            `apply` optional (parseSchemaType "ec")
    schemaTypeToXML s x@Points{} =
        toXMLElement s []
            [ schemaTypeToXML "v" $ points_v x
            , schemaTypeToXML "d" $ points_d x
            , maybe [] (schemaTypeToXML "p") $ points_p x
            , maybe [] (schemaTypeToXML "n") $ points_n x
            , maybe [] (schemaTypeToXML "tc") $ points_tc x
            , maybe [] (schemaTypeToXML "iv") $ points_iv x
            , maybe [] (schemaTypeToXML "ec") $ points_ec x
            ]
 
data Contexte = Contexte
        { contexte_etapeMetier :: Xsd.XsdString
        , contexte_contexteReleve :: Xsd.XsdString
        , contexte_typeReleve :: Xsd.XsdString
        , contexte_motifReleve :: Maybe Xsd.XsdString
        , contexte_grandeur :: [GrandeurType]
        }
        deriving (Eq,Show)
instance SchemaType Contexte where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return Contexte
            `apply` parseSchemaType "etapeMetier"
            `apply` parseSchemaType "contexteReleve"
            `apply` parseSchemaType "typeReleve"
            `apply` optional (parseSchemaType "motifReleve")
            `apply` many1 (parseSchemaType "grandeur")
    schemaTypeToXML s x@Contexte{} =
        toXMLElement s []
            [ schemaTypeToXML "etapeMetier" $ contexte_etapeMetier x
            , schemaTypeToXML "contexteReleve" $ contexte_contexteReleve x
            , schemaTypeToXML "typeReleve" $ contexte_typeReleve x
            , maybe [] (schemaTypeToXML "motifReleve") $ contexte_motifReleve x
            , concatMap (schemaTypeToXML "grandeur") $ contexte_grandeur x
            ]
 
data Calendrier = Calendrier
        { calendrier_idCalendrier :: Xsd.XsdString
        , calendrier_libelleCalendrier :: Xsd.XsdString
        , calendrier_classeTemporelle :: [ClasseTemporelle]
        }
        deriving (Eq,Show)
instance SchemaType Calendrier where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return Calendrier
            `apply` parseSchemaType "idCalendrier"
            `apply` parseSchemaType "libelleCalendrier"
            `apply` many1 (parseSchemaType "classeTemporelle")
    schemaTypeToXML s x@Calendrier{} =
        toXMLElement s []
            [ schemaTypeToXML "idCalendrier" $ calendrier_idCalendrier x
            , schemaTypeToXML "libelleCalendrier" $ calendrier_libelleCalendrier x
            , concatMap (schemaTypeToXML "classeTemporelle") $ calendrier_classeTemporelle x
            ]
 
data ClasseTemporelle = ClasseTemporelle
        { classeTemporelle_idClasseTemporelle :: Maybe Xsd.XsdString
        , classeTemporelle_libelleClasseTemporelle :: Maybe Xsd.XsdString
        , classeTemporelle_codeCadran :: Maybe Xsd.XsdString
        , classeTemporelle_valeur :: [Valeur]
        }
        deriving (Eq,Show)
instance SchemaType ClasseTemporelle where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ClasseTemporelle
            `apply` optional (parseSchemaType "idClasseTemporelle")
            `apply` optional (parseSchemaType "libelleClasseTemporelle")
            `apply` optional (parseSchemaType "codeCadran")
            `apply` many (parseSchemaType "valeur")
    schemaTypeToXML s x@ClasseTemporelle{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "idClasseTemporelle") $ classeTemporelle_idClasseTemporelle x
            , maybe [] (schemaTypeToXML "libelleClasseTemporelle") $ classeTemporelle_libelleClasseTemporelle x
            , maybe [] (schemaTypeToXML "codeCadran") $ classeTemporelle_codeCadran x
            , concatMap (schemaTypeToXML "valeur") $ classeTemporelle_valeur x
            ]
 
data ClasseTemporelleTotalisateur = ClasseTemporelleTotalisateur
        { classeTemporelleTotalisateur_codeCadran :: Maybe Xsd.XsdString
        , classeTemporelleTotalisateur_valeur :: [Valeur]
        }
        deriving (Eq,Show)
instance SchemaType ClasseTemporelleTotalisateur where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ClasseTemporelleTotalisateur
            `apply` optional (parseSchemaType "codeCadran")
            `apply` many (parseSchemaType "valeur")
    schemaTypeToXML s x@ClasseTemporelleTotalisateur{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "codeCadran") $ classeTemporelleTotalisateur_codeCadran x
            , concatMap (schemaTypeToXML "valeur") $ classeTemporelleTotalisateur_valeur x
            ]
 
data Valeur = Valeur
        { valeur_d :: Xsd.XsdString
        , valeur_v :: Xsd.Integer
        , valeur_iv :: Xsd.Integer
        }
        deriving (Eq,Show)
instance SchemaType Valeur where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return Valeur
            `apply` parseSchemaType "d"
            `apply` parseSchemaType "v"
            `apply` parseSchemaType "iv"
    schemaTypeToXML s x@Valeur{} =
        toXMLElement s []
            [ schemaTypeToXML "d" $ valeur_d x
            , schemaTypeToXML "v" $ valeur_v x
            , schemaTypeToXML "iv" $ valeur_iv x
            ]
