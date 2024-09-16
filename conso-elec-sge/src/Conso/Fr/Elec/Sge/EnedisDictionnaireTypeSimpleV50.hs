{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
  ( module Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
    ( parseSimpleType,
      toXMLElement,
      toXMLText,
      element,
      interior,
      literal,
      onFail,
      Commitment(commit) )
import Text.XML.HaXml.OneOfN ()
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
newtype AcceptabiliteResultatCodeType = AcceptabiliteResultatCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AcceptabiliteResultatCodeType Xsd.XsdString where
    restricts (AcceptabiliteResultatCodeType x) = x
instance SchemaType AcceptabiliteResultatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AcceptabiliteResultatCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AcceptabiliteResultatCodeType where
    acceptingParser = fmap AcceptabiliteResultatCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AcceptabiliteResultatCodeType x) = simpleTypeText x
 
newtype AcheminementTarifCodeType = AcheminementTarifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AcheminementTarifCodeType Xsd.XsdString where
    restricts (AcheminementTarifCodeType x) = x
instance SchemaType AcheminementTarifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AcheminementTarifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AcheminementTarifCodeType where
    acceptingParser = fmap AcheminementTarifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AcheminementTarifCodeType x) = simpleTypeText x
 
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
 
newtype ActiviteCodeNafType = ActiviteCodeNafType Xsd.XsdString deriving (Eq,Show)
instance Restricts ActiviteCodeNafType Xsd.XsdString where
    restricts (ActiviteCodeNafType x) = x
instance SchemaType ActiviteCodeNafType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ActiviteCodeNafType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ActiviteCodeNafType where
    acceptingParser = fmap ActiviteCodeNafType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9A-Za-z]{1,5})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (ActiviteCodeNafType x) = simpleTypeText x
 
newtype ActiviteSecteurCodeType = ActiviteSecteurCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ActiviteSecteurCodeType Xsd.XsdString where
    restricts (ActiviteSecteurCodeType x) = x
instance SchemaType ActiviteSecteurCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ActiviteSecteurCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ActiviteSecteurCodeType where
    acceptingParser = fmap ActiviteSecteurCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ActiviteSecteurCodeType x) = simpleTypeText x
 
newtype AdresseAfnorLigneType = AdresseAfnorLigneType Xsd.XsdString deriving (Eq,Show)
instance Restricts AdresseAfnorLigneType Xsd.XsdString where
    restricts (AdresseAfnorLigneType x) = x
instance SchemaType AdresseAfnorLigneType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AdresseAfnorLigneType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AdresseAfnorLigneType where
    acceptingParser = fmap AdresseAfnorLigneType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AdresseAfnorLigneType x) = simpleTypeText x
 
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
    --      (Pattern [0-9a-zA-Z][\-._0-9a-zA-Z]{0,255}@[0-9a-zA-Z][\-._0-9a-zA-Z]{1,255}.[a-zA-Z]{2,63})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (AdresseEmailType x) = simpleTypeText x
 
newtype AffaireAnnulationMotifCodeType = AffaireAnnulationMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireAnnulationMotifCodeType Xsd.XsdString where
    restricts (AffaireAnnulationMotifCodeType x) = x
instance SchemaType AffaireAnnulationMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireAnnulationMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireAnnulationMotifCodeType where
    acceptingParser = fmap AffaireAnnulationMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireAnnulationMotifCodeType x) = simpleTypeText x
 
newtype AffaireAnnulationMotifRefusCodeType = AffaireAnnulationMotifRefusCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireAnnulationMotifRefusCodeType Xsd.XsdString where
    restricts (AffaireAnnulationMotifRefusCodeType x) = x
instance SchemaType AffaireAnnulationMotifRefusCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireAnnulationMotifRefusCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireAnnulationMotifRefusCodeType where
    acceptingParser = fmap AffaireAnnulationMotifRefusCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireAnnulationMotifRefusCodeType x) = simpleTypeText x
 
newtype AffaireClotureMotifCodeType = AffaireClotureMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireClotureMotifCodeType Xsd.XsdString where
    restricts (AffaireClotureMotifCodeType x) = x
instance SchemaType AffaireClotureMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireClotureMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireClotureMotifCodeType where
    acceptingParser = fmap AffaireClotureMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireClotureMotifCodeType x) = simpleTypeText x
 
newtype AffaireEtatCodeType = AffaireEtatCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireEtatCodeType Xsd.XsdString where
    restricts (AffaireEtatCodeType x) = x
instance SchemaType AffaireEtatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireEtatCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireEtatCodeType where
    acceptingParser = fmap AffaireEtatCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AffaireEtatCodeType x) = simpleTypeText x
 
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
 
newtype AffaireModificationMotifRefusCodeType = AffaireModificationMotifRefusCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireModificationMotifRefusCodeType Xsd.XsdString where
    restricts (AffaireModificationMotifRefusCodeType x) = x
instance SchemaType AffaireModificationMotifRefusCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireModificationMotifRefusCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireModificationMotifRefusCodeType where
    acceptingParser = fmap AffaireModificationMotifRefusCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (AffaireModificationMotifRefusCodeType x) = simpleTypeText x
 
newtype AffaireNatureModificationCodeType = AffaireNatureModificationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AffaireNatureModificationCodeType Xsd.XsdString where
    restricts (AffaireNatureModificationCodeType x) = x
instance SchemaType AffaireNatureModificationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AffaireNatureModificationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireNatureModificationCodeType where
    acceptingParser = fmap AffaireNatureModificationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (AffaireNatureModificationCodeType x) = simpleTypeText x
 
data AffaireStatutCodeType
    = AffaireStatutCodeType_ANNUL
    | AffaireStatutCodeType_COURS
    | AffaireStatutCodeType_TERMN
    deriving (Eq,Show,Enum)
instance SchemaType AffaireStatutCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AffaireStatutCodeType where
    acceptingParser =  do _ <- literal "ANNUL"; return AffaireStatutCodeType_ANNUL
                      `onFail` do _ <- literal "COURS"; return AffaireStatutCodeType_COURS
                      `onFail` do _ <- literal "TERMN"; return AffaireStatutCodeType_TERMN
                      
    simpleTypeText AffaireStatutCodeType_ANNUL = "ANNUL"
    simpleTypeText AffaireStatutCodeType_COURS = "COURS"
    simpleTypeText AffaireStatutCodeType_TERMN = "TERMN"
 
data AlimentationEtatCodeType
    = AlimentationEtatCodeType_ALIM
    | AlimentationEtatCodeType_COUP
    | AlimentationEtatCodeType_LIMI
    | AlimentationEtatCodeType_NRAC
    | AlimentationEtatCodeType_NALI
    deriving (Eq,Show,Enum)
instance SchemaType AlimentationEtatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AlimentationEtatCodeType where
    acceptingParser =  do _ <- literal "ALIM"; return AlimentationEtatCodeType_ALIM
                      `onFail` do _ <- literal "COUP"; return AlimentationEtatCodeType_COUP
                      `onFail` do _ <- literal "LIMI"; return AlimentationEtatCodeType_LIMI
                      `onFail` do _ <- literal "NRAC"; return AlimentationEtatCodeType_NRAC
                      `onFail` do _ <- literal "NALI"; return AlimentationEtatCodeType_NALI
                      
    simpleTypeText AlimentationEtatCodeType_ALIM = "ALIM"
    simpleTypeText AlimentationEtatCodeType_COUP = "COUP"
    simpleTypeText AlimentationEtatCodeType_LIMI = "LIMI"
    simpleTypeText AlimentationEtatCodeType_NRAC = "NRAC"
    simpleTypeText AlimentationEtatCodeType_NALI = "NALI"
 
newtype AlimentationModeApresCompteurCodeType = AlimentationModeApresCompteurCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AlimentationModeApresCompteurCodeType Xsd.XsdString where
    restricts (AlimentationModeApresCompteurCodeType x) = x
instance SchemaType AlimentationModeApresCompteurCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AlimentationModeApresCompteurCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AlimentationModeApresCompteurCodeType where
    acceptingParser = fmap AlimentationModeApresCompteurCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AlimentationModeApresCompteurCodeType x) = simpleTypeText x
 
newtype AlimentationSecoursModeBasculeCodeType = AlimentationSecoursModeBasculeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts AlimentationSecoursModeBasculeCodeType Xsd.XsdString where
    restricts (AlimentationSecoursModeBasculeCodeType x) = x
instance SchemaType AlimentationSecoursModeBasculeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AlimentationSecoursModeBasculeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AlimentationSecoursModeBasculeCodeType where
    acceptingParser = fmap AlimentationSecoursModeBasculeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (AlimentationSecoursModeBasculeCodeType x) = simpleTypeText x
 
newtype AnneeType = AnneeType Xsd.GYear deriving (Eq,Show)
instance Restricts AnneeType Xsd.GYear where
    restricts (AnneeType x) = x
instance SchemaType AnneeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (AnneeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType AnneeType where
    acceptingParser = fmap AnneeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (AnneeType x) = simpleTypeText x
 
newtype ApplicationCodeType = ApplicationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ApplicationCodeType Xsd.XsdString where
    restricts (ApplicationCodeType x) = x
instance SchemaType ApplicationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ApplicationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ApplicationCodeType where
    acceptingParser = fmap ApplicationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ApplicationCodeType x) = simpleTypeText x
 
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
 
newtype CalendrierCodeType = CalendrierCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CalendrierCodeType Xsd.XsdString where
    restricts (CalendrierCodeType x) = x
instance SchemaType CalendrierCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CalendrierCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CalendrierCodeType where
    acceptingParser = fmap CalendrierCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CalendrierCodeType x) = simpleTypeText x
 
newtype CalendrierJourTypeCategorieCodeType = CalendrierJourTypeCategorieCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CalendrierJourTypeCategorieCodeType Xsd.XsdString where
    restricts (CalendrierJourTypeCategorieCodeType x) = x
instance SchemaType CalendrierJourTypeCategorieCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CalendrierJourTypeCategorieCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CalendrierJourTypeCategorieCodeType where
    acceptingParser = fmap CalendrierJourTypeCategorieCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z1-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CalendrierJourTypeCategorieCodeType x) = simpleTypeText x
 
newtype CalendrierJourTypeCodeType = CalendrierJourTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CalendrierJourTypeCodeType Xsd.XsdString where
    restricts (CalendrierJourTypeCodeType x) = x
instance SchemaType CalendrierJourTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CalendrierJourTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CalendrierJourTypeCodeType where
    acceptingParser = fmap CalendrierJourTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z1-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CalendrierJourTypeCodeType x) = simpleTypeText x
 
data CentreGeographiqueCodeType
    = CentreGeographiqueCodeType_V11
    | CentreGeographiqueCodeType_V12
    | CentreGeographiqueCodeType_V121
    | CentreGeographiqueCodeType_V122
    | CentreGeographiqueCodeType_V124
    | CentreGeographiqueCodeType_V125
    | CentreGeographiqueCodeType_V13
    | CentreGeographiqueCodeType_V14
    | CentreGeographiqueCodeType_V142
    | CentreGeographiqueCodeType_V143
    | CentreGeographiqueCodeType_V144
    | CentreGeographiqueCodeType_V145
    | CentreGeographiqueCodeType_V146
    | CentreGeographiqueCodeType_V147
    | CentreGeographiqueCodeType_V148
    | CentreGeographiqueCodeType_V15
    | CentreGeographiqueCodeType_V151
    | CentreGeographiqueCodeType_V152
    | CentreGeographiqueCodeType_V154
    | CentreGeographiqueCodeType_V155
    | CentreGeographiqueCodeType_V16
    | CentreGeographiqueCodeType_V161
    | CentreGeographiqueCodeType_V162
    | CentreGeographiqueCodeType_V163
    | CentreGeographiqueCodeType_V164
    | CentreGeographiqueCodeType_V165
    | CentreGeographiqueCodeType_V171
    | CentreGeographiqueCodeType_V172
    | CentreGeographiqueCodeType_V173
    | CentreGeographiqueCodeType_V175
    | CentreGeographiqueCodeType_V176
    | CentreGeographiqueCodeType_V177
    | CentreGeographiqueCodeType_V179
    | CentreGeographiqueCodeType_V191
    | CentreGeographiqueCodeType_V193
    | CentreGeographiqueCodeType_V194
    | CentreGeographiqueCodeType_V195
    | CentreGeographiqueCodeType_V196
    | CentreGeographiqueCodeType_V197
    | CentreGeographiqueCodeType_V198
    | CentreGeographiqueCodeType_V199
    | CentreGeographiqueCodeType_V21
    | CentreGeographiqueCodeType_V211
    | CentreGeographiqueCodeType_V212
    | CentreGeographiqueCodeType_V213
    | CentreGeographiqueCodeType_V214
    | CentreGeographiqueCodeType_V215
    | CentreGeographiqueCodeType_V22
    | CentreGeographiqueCodeType_V221
    | CentreGeographiqueCodeType_V222
    | CentreGeographiqueCodeType_V223
    | CentreGeographiqueCodeType_V224
    | CentreGeographiqueCodeType_V225
    | CentreGeographiqueCodeType_V23
    | CentreGeographiqueCodeType_V231
    | CentreGeographiqueCodeType_V232
    | CentreGeographiqueCodeType_V233
    | CentreGeographiqueCodeType_V234
    | CentreGeographiqueCodeType_V235
    | CentreGeographiqueCodeType_V24
    | CentreGeographiqueCodeType_V241
    | CentreGeographiqueCodeType_V242
    | CentreGeographiqueCodeType_V243
    | CentreGeographiqueCodeType_V245
    | CentreGeographiqueCodeType_V25
    | CentreGeographiqueCodeType_V251
    | CentreGeographiqueCodeType_V252
    | CentreGeographiqueCodeType_V253
    | CentreGeographiqueCodeType_V254
    | CentreGeographiqueCodeType_V256
    | CentreGeographiqueCodeType_V258
    | CentreGeographiqueCodeType_V259
    | CentreGeographiqueCodeType_V26
    | CentreGeographiqueCodeType_V41
    | CentreGeographiqueCodeType_V42
    | CentreGeographiqueCodeType_V43
    | CentreGeographiqueCodeType_V45
    | CentreGeographiqueCodeType_V51
    | CentreGeographiqueCodeType_V52
    | CentreGeographiqueCodeType_V54
    | CentreGeographiqueCodeType_V55
    | CentreGeographiqueCodeType_V56
    | CentreGeographiqueCodeType_V63
    | CentreGeographiqueCodeType_V64
    | CentreGeographiqueCodeType_V65
    | CentreGeographiqueCodeType_V71
    | CentreGeographiqueCodeType_V72
    | CentreGeographiqueCodeType_V73
    | CentreGeographiqueCodeType_V74
    | CentreGeographiqueCodeType_V75
    | CentreGeographiqueCodeType_V91
    | CentreGeographiqueCodeType_V92
    | CentreGeographiqueCodeType_V93
    | CentreGeographiqueCodeType_V94
    | CentreGeographiqueCodeType_V95
    | CentreGeographiqueCodeType_V96
    | CentreGeographiqueCodeType_V97
    deriving (Eq,Show,Enum)
instance SchemaType CentreGeographiqueCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CentreGeographiqueCodeType where
    acceptingParser =  do _ <- literal "11"; return CentreGeographiqueCodeType_V11
                      `onFail` do _ <- literal "12"; return CentreGeographiqueCodeType_V12
                      `onFail` do _ <- literal "121"; return CentreGeographiqueCodeType_V121
                      `onFail` do _ <- literal "122"; return CentreGeographiqueCodeType_V122
                      `onFail` do _ <- literal "124"; return CentreGeographiqueCodeType_V124
                      `onFail` do _ <- literal "125"; return CentreGeographiqueCodeType_V125
                      `onFail` do _ <- literal "13"; return CentreGeographiqueCodeType_V13
                      `onFail` do _ <- literal "14"; return CentreGeographiqueCodeType_V14
                      `onFail` do _ <- literal "142"; return CentreGeographiqueCodeType_V142
                      `onFail` do _ <- literal "143"; return CentreGeographiqueCodeType_V143
                      `onFail` do _ <- literal "144"; return CentreGeographiqueCodeType_V144
                      `onFail` do _ <- literal "145"; return CentreGeographiqueCodeType_V145
                      `onFail` do _ <- literal "146"; return CentreGeographiqueCodeType_V146
                      `onFail` do _ <- literal "147"; return CentreGeographiqueCodeType_V147
                      `onFail` do _ <- literal "148"; return CentreGeographiqueCodeType_V148
                      `onFail` do _ <- literal "15"; return CentreGeographiqueCodeType_V15
                      `onFail` do _ <- literal "151"; return CentreGeographiqueCodeType_V151
                      `onFail` do _ <- literal "152"; return CentreGeographiqueCodeType_V152
                      `onFail` do _ <- literal "154"; return CentreGeographiqueCodeType_V154
                      `onFail` do _ <- literal "155"; return CentreGeographiqueCodeType_V155
                      `onFail` do _ <- literal "16"; return CentreGeographiqueCodeType_V16
                      `onFail` do _ <- literal "161"; return CentreGeographiqueCodeType_V161
                      `onFail` do _ <- literal "162"; return CentreGeographiqueCodeType_V162
                      `onFail` do _ <- literal "163"; return CentreGeographiqueCodeType_V163
                      `onFail` do _ <- literal "164"; return CentreGeographiqueCodeType_V164
                      `onFail` do _ <- literal "165"; return CentreGeographiqueCodeType_V165
                      `onFail` do _ <- literal "171"; return CentreGeographiqueCodeType_V171
                      `onFail` do _ <- literal "172"; return CentreGeographiqueCodeType_V172
                      `onFail` do _ <- literal "173"; return CentreGeographiqueCodeType_V173
                      `onFail` do _ <- literal "175"; return CentreGeographiqueCodeType_V175
                      `onFail` do _ <- literal "176"; return CentreGeographiqueCodeType_V176
                      `onFail` do _ <- literal "177"; return CentreGeographiqueCodeType_V177
                      `onFail` do _ <- literal "179"; return CentreGeographiqueCodeType_V179
                      `onFail` do _ <- literal "191"; return CentreGeographiqueCodeType_V191
                      `onFail` do _ <- literal "193"; return CentreGeographiqueCodeType_V193
                      `onFail` do _ <- literal "194"; return CentreGeographiqueCodeType_V194
                      `onFail` do _ <- literal "195"; return CentreGeographiqueCodeType_V195
                      `onFail` do _ <- literal "196"; return CentreGeographiqueCodeType_V196
                      `onFail` do _ <- literal "197"; return CentreGeographiqueCodeType_V197
                      `onFail` do _ <- literal "198"; return CentreGeographiqueCodeType_V198
                      `onFail` do _ <- literal "199"; return CentreGeographiqueCodeType_V199
                      `onFail` do _ <- literal "21"; return CentreGeographiqueCodeType_V21
                      `onFail` do _ <- literal "211"; return CentreGeographiqueCodeType_V211
                      `onFail` do _ <- literal "212"; return CentreGeographiqueCodeType_V212
                      `onFail` do _ <- literal "213"; return CentreGeographiqueCodeType_V213
                      `onFail` do _ <- literal "214"; return CentreGeographiqueCodeType_V214
                      `onFail` do _ <- literal "215"; return CentreGeographiqueCodeType_V215
                      `onFail` do _ <- literal "22"; return CentreGeographiqueCodeType_V22
                      `onFail` do _ <- literal "221"; return CentreGeographiqueCodeType_V221
                      `onFail` do _ <- literal "222"; return CentreGeographiqueCodeType_V222
                      `onFail` do _ <- literal "223"; return CentreGeographiqueCodeType_V223
                      `onFail` do _ <- literal "224"; return CentreGeographiqueCodeType_V224
                      `onFail` do _ <- literal "225"; return CentreGeographiqueCodeType_V225
                      `onFail` do _ <- literal "23"; return CentreGeographiqueCodeType_V23
                      `onFail` do _ <- literal "231"; return CentreGeographiqueCodeType_V231
                      `onFail` do _ <- literal "232"; return CentreGeographiqueCodeType_V232
                      `onFail` do _ <- literal "233"; return CentreGeographiqueCodeType_V233
                      `onFail` do _ <- literal "234"; return CentreGeographiqueCodeType_V234
                      `onFail` do _ <- literal "235"; return CentreGeographiqueCodeType_V235
                      `onFail` do _ <- literal "24"; return CentreGeographiqueCodeType_V24
                      `onFail` do _ <- literal "241"; return CentreGeographiqueCodeType_V241
                      `onFail` do _ <- literal "242"; return CentreGeographiqueCodeType_V242
                      `onFail` do _ <- literal "243"; return CentreGeographiqueCodeType_V243
                      `onFail` do _ <- literal "245"; return CentreGeographiqueCodeType_V245
                      `onFail` do _ <- literal "25"; return CentreGeographiqueCodeType_V25
                      `onFail` do _ <- literal "251"; return CentreGeographiqueCodeType_V251
                      `onFail` do _ <- literal "252"; return CentreGeographiqueCodeType_V252
                      `onFail` do _ <- literal "253"; return CentreGeographiqueCodeType_V253
                      `onFail` do _ <- literal "254"; return CentreGeographiqueCodeType_V254
                      `onFail` do _ <- literal "256"; return CentreGeographiqueCodeType_V256
                      `onFail` do _ <- literal "258"; return CentreGeographiqueCodeType_V258
                      `onFail` do _ <- literal "259"; return CentreGeographiqueCodeType_V259
                      `onFail` do _ <- literal "26"; return CentreGeographiqueCodeType_V26
                      `onFail` do _ <- literal "41"; return CentreGeographiqueCodeType_V41
                      `onFail` do _ <- literal "42"; return CentreGeographiqueCodeType_V42
                      `onFail` do _ <- literal "43"; return CentreGeographiqueCodeType_V43
                      `onFail` do _ <- literal "45"; return CentreGeographiqueCodeType_V45
                      `onFail` do _ <- literal "51"; return CentreGeographiqueCodeType_V51
                      `onFail` do _ <- literal "52"; return CentreGeographiqueCodeType_V52
                      `onFail` do _ <- literal "54"; return CentreGeographiqueCodeType_V54
                      `onFail` do _ <- literal "55"; return CentreGeographiqueCodeType_V55
                      `onFail` do _ <- literal "56"; return CentreGeographiqueCodeType_V56
                      `onFail` do _ <- literal "63"; return CentreGeographiqueCodeType_V63
                      `onFail` do _ <- literal "64"; return CentreGeographiqueCodeType_V64
                      `onFail` do _ <- literal "65"; return CentreGeographiqueCodeType_V65
                      `onFail` do _ <- literal "71"; return CentreGeographiqueCodeType_V71
                      `onFail` do _ <- literal "72"; return CentreGeographiqueCodeType_V72
                      `onFail` do _ <- literal "73"; return CentreGeographiqueCodeType_V73
                      `onFail` do _ <- literal "74"; return CentreGeographiqueCodeType_V74
                      `onFail` do _ <- literal "75"; return CentreGeographiqueCodeType_V75
                      `onFail` do _ <- literal "91"; return CentreGeographiqueCodeType_V91
                      `onFail` do _ <- literal "92"; return CentreGeographiqueCodeType_V92
                      `onFail` do _ <- literal "93"; return CentreGeographiqueCodeType_V93
                      `onFail` do _ <- literal "94"; return CentreGeographiqueCodeType_V94
                      `onFail` do _ <- literal "95"; return CentreGeographiqueCodeType_V95
                      `onFail` do _ <- literal "96"; return CentreGeographiqueCodeType_V96
                      `onFail` do _ <- literal "97"; return CentreGeographiqueCodeType_V97
                      
    simpleTypeText CentreGeographiqueCodeType_V11 = "11"
    simpleTypeText CentreGeographiqueCodeType_V12 = "12"
    simpleTypeText CentreGeographiqueCodeType_V121 = "121"
    simpleTypeText CentreGeographiqueCodeType_V122 = "122"
    simpleTypeText CentreGeographiqueCodeType_V124 = "124"
    simpleTypeText CentreGeographiqueCodeType_V125 = "125"
    simpleTypeText CentreGeographiqueCodeType_V13 = "13"
    simpleTypeText CentreGeographiqueCodeType_V14 = "14"
    simpleTypeText CentreGeographiqueCodeType_V142 = "142"
    simpleTypeText CentreGeographiqueCodeType_V143 = "143"
    simpleTypeText CentreGeographiqueCodeType_V144 = "144"
    simpleTypeText CentreGeographiqueCodeType_V145 = "145"
    simpleTypeText CentreGeographiqueCodeType_V146 = "146"
    simpleTypeText CentreGeographiqueCodeType_V147 = "147"
    simpleTypeText CentreGeographiqueCodeType_V148 = "148"
    simpleTypeText CentreGeographiqueCodeType_V15 = "15"
    simpleTypeText CentreGeographiqueCodeType_V151 = "151"
    simpleTypeText CentreGeographiqueCodeType_V152 = "152"
    simpleTypeText CentreGeographiqueCodeType_V154 = "154"
    simpleTypeText CentreGeographiqueCodeType_V155 = "155"
    simpleTypeText CentreGeographiqueCodeType_V16 = "16"
    simpleTypeText CentreGeographiqueCodeType_V161 = "161"
    simpleTypeText CentreGeographiqueCodeType_V162 = "162"
    simpleTypeText CentreGeographiqueCodeType_V163 = "163"
    simpleTypeText CentreGeographiqueCodeType_V164 = "164"
    simpleTypeText CentreGeographiqueCodeType_V165 = "165"
    simpleTypeText CentreGeographiqueCodeType_V171 = "171"
    simpleTypeText CentreGeographiqueCodeType_V172 = "172"
    simpleTypeText CentreGeographiqueCodeType_V173 = "173"
    simpleTypeText CentreGeographiqueCodeType_V175 = "175"
    simpleTypeText CentreGeographiqueCodeType_V176 = "176"
    simpleTypeText CentreGeographiqueCodeType_V177 = "177"
    simpleTypeText CentreGeographiqueCodeType_V179 = "179"
    simpleTypeText CentreGeographiqueCodeType_V191 = "191"
    simpleTypeText CentreGeographiqueCodeType_V193 = "193"
    simpleTypeText CentreGeographiqueCodeType_V194 = "194"
    simpleTypeText CentreGeographiqueCodeType_V195 = "195"
    simpleTypeText CentreGeographiqueCodeType_V196 = "196"
    simpleTypeText CentreGeographiqueCodeType_V197 = "197"
    simpleTypeText CentreGeographiqueCodeType_V198 = "198"
    simpleTypeText CentreGeographiqueCodeType_V199 = "199"
    simpleTypeText CentreGeographiqueCodeType_V21 = "21"
    simpleTypeText CentreGeographiqueCodeType_V211 = "211"
    simpleTypeText CentreGeographiqueCodeType_V212 = "212"
    simpleTypeText CentreGeographiqueCodeType_V213 = "213"
    simpleTypeText CentreGeographiqueCodeType_V214 = "214"
    simpleTypeText CentreGeographiqueCodeType_V215 = "215"
    simpleTypeText CentreGeographiqueCodeType_V22 = "22"
    simpleTypeText CentreGeographiqueCodeType_V221 = "221"
    simpleTypeText CentreGeographiqueCodeType_V222 = "222"
    simpleTypeText CentreGeographiqueCodeType_V223 = "223"
    simpleTypeText CentreGeographiqueCodeType_V224 = "224"
    simpleTypeText CentreGeographiqueCodeType_V225 = "225"
    simpleTypeText CentreGeographiqueCodeType_V23 = "23"
    simpleTypeText CentreGeographiqueCodeType_V231 = "231"
    simpleTypeText CentreGeographiqueCodeType_V232 = "232"
    simpleTypeText CentreGeographiqueCodeType_V233 = "233"
    simpleTypeText CentreGeographiqueCodeType_V234 = "234"
    simpleTypeText CentreGeographiqueCodeType_V235 = "235"
    simpleTypeText CentreGeographiqueCodeType_V24 = "24"
    simpleTypeText CentreGeographiqueCodeType_V241 = "241"
    simpleTypeText CentreGeographiqueCodeType_V242 = "242"
    simpleTypeText CentreGeographiqueCodeType_V243 = "243"
    simpleTypeText CentreGeographiqueCodeType_V245 = "245"
    simpleTypeText CentreGeographiqueCodeType_V25 = "25"
    simpleTypeText CentreGeographiqueCodeType_V251 = "251"
    simpleTypeText CentreGeographiqueCodeType_V252 = "252"
    simpleTypeText CentreGeographiqueCodeType_V253 = "253"
    simpleTypeText CentreGeographiqueCodeType_V254 = "254"
    simpleTypeText CentreGeographiqueCodeType_V256 = "256"
    simpleTypeText CentreGeographiqueCodeType_V258 = "258"
    simpleTypeText CentreGeographiqueCodeType_V259 = "259"
    simpleTypeText CentreGeographiqueCodeType_V26 = "26"
    simpleTypeText CentreGeographiqueCodeType_V41 = "41"
    simpleTypeText CentreGeographiqueCodeType_V42 = "42"
    simpleTypeText CentreGeographiqueCodeType_V43 = "43"
    simpleTypeText CentreGeographiqueCodeType_V45 = "45"
    simpleTypeText CentreGeographiqueCodeType_V51 = "51"
    simpleTypeText CentreGeographiqueCodeType_V52 = "52"
    simpleTypeText CentreGeographiqueCodeType_V54 = "54"
    simpleTypeText CentreGeographiqueCodeType_V55 = "55"
    simpleTypeText CentreGeographiqueCodeType_V56 = "56"
    simpleTypeText CentreGeographiqueCodeType_V63 = "63"
    simpleTypeText CentreGeographiqueCodeType_V64 = "64"
    simpleTypeText CentreGeographiqueCodeType_V65 = "65"
    simpleTypeText CentreGeographiqueCodeType_V71 = "71"
    simpleTypeText CentreGeographiqueCodeType_V72 = "72"
    simpleTypeText CentreGeographiqueCodeType_V73 = "73"
    simpleTypeText CentreGeographiqueCodeType_V74 = "74"
    simpleTypeText CentreGeographiqueCodeType_V75 = "75"
    simpleTypeText CentreGeographiqueCodeType_V91 = "91"
    simpleTypeText CentreGeographiqueCodeType_V92 = "92"
    simpleTypeText CentreGeographiqueCodeType_V93 = "93"
    simpleTypeText CentreGeographiqueCodeType_V94 = "94"
    simpleTypeText CentreGeographiqueCodeType_V95 = "95"
    simpleTypeText CentreGeographiqueCodeType_V96 = "96"
    simpleTypeText CentreGeographiqueCodeType_V97 = "97"
 
newtype Chaine15Type = Chaine15Type Xsd.XsdString deriving (Eq,Show)
instance Restricts Chaine15Type Xsd.XsdString where
    restricts (Chaine15Type x) = x
instance SchemaType Chaine15Type where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (Chaine15Type x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Chaine15Type where
    acceptingParser = fmap Chaine15Type acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (Chaine15Type x) = simpleTypeText x
 
newtype Chaine2047Type = Chaine2047Type Xsd.XsdString deriving (Eq,Show)
instance Restricts Chaine2047Type Xsd.XsdString where
    restricts (Chaine2047Type x) = x
instance SchemaType Chaine2047Type where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (Chaine2047Type x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Chaine2047Type where
    acceptingParser = fmap Chaine2047Type acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (Chaine2047Type x) = simpleTypeText x
 
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
 
newtype CircuitTempoCodeType = CircuitTempoCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CircuitTempoCodeType Xsd.XsdString where
    restricts (CircuitTempoCodeType x) = x
instance SchemaType CircuitTempoCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CircuitTempoCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CircuitTempoCodeType where
    acceptingParser = fmap CircuitTempoCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CircuitTempoCodeType x) = simpleTypeText x
 
newtype CiviliteAbreviationType = CiviliteAbreviationType Xsd.XsdString deriving (Eq,Show)
instance Restricts CiviliteAbreviationType Xsd.XsdString where
    restricts (CiviliteAbreviationType x) = x
instance SchemaType CiviliteAbreviationType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CiviliteAbreviationType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CiviliteAbreviationType where
    acceptingParser = fmap CiviliteAbreviationType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CiviliteAbreviationType x) = simpleTypeText x
 
newtype ClasseTemporelleCodeType = ClasseTemporelleCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ClasseTemporelleCodeType Xsd.XsdString where
    restricts (ClasseTemporelleCodeType x) = x
instance SchemaType ClasseTemporelleCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ClasseTemporelleCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ClasseTemporelleCodeType where
    acceptingParser = fmap ClasseTemporelleCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ClasseTemporelleCodeType x) = simpleTypeText x
 
newtype CleTeleAccesType = CleTeleAccesType Xsd.XsdString deriving (Eq,Show)
instance Restricts CleTeleAccesType Xsd.XsdString where
    restricts (CleTeleAccesType x) = x
instance SchemaType CleTeleAccesType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CleTeleAccesType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CleTeleAccesType where
    acceptingParser = fmap CleTeleAccesType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CleTeleAccesType x) = simpleTypeText x
 
data ClientFinalCategorieCodeType
    = ClientFinalCategorieCodeType_PRO
    | ClientFinalCategorieCodeType_RES
    deriving (Eq,Show,Enum)
instance SchemaType ClientFinalCategorieCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ClientFinalCategorieCodeType where
    acceptingParser =  do _ <- literal "PRO"; return ClientFinalCategorieCodeType_PRO
                      `onFail` do _ <-literal "RES"; return ClientFinalCategorieCodeType_RES
                      
    simpleTypeText ClientFinalCategorieCodeType_PRO = "PRO"
    simpleTypeText ClientFinalCategorieCodeType_RES = "RES"
 
newtype ClientPrioritaireTypeCodeType = ClientPrioritaireTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ClientPrioritaireTypeCodeType Xsd.XsdString where
    restricts (ClientPrioritaireTypeCodeType x) = x
instance SchemaType ClientPrioritaireTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ClientPrioritaireTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ClientPrioritaireTypeCodeType where
    acceptingParser = fmap ClientPrioritaireTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ClientPrioritaireTypeCodeType x) = simpleTypeText x
 
newtype CodeEicType = CodeEicType Xsd.XsdString deriving (Eq,Show)
instance Restricts CodeEicType Xsd.XsdString where
    restricts (CodeEicType x) = x
instance SchemaType CodeEicType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CodeEicType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CodeEicType where
    acceptingParser = fmap CodeEicType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CodeEicType x) = simpleTypeText x
 
newtype CodePostalFrancaisType = CodePostalFrancaisType Xsd.XsdString deriving (Eq,Show)
instance Restricts CodePostalFrancaisType Xsd.XsdString where
    restricts (CodePostalFrancaisType x) = x
instance SchemaType CodePostalFrancaisType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CodePostalFrancaisType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CodePostalFrancaisType where
    acceptingParser = fmap CodePostalFrancaisType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{5})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CodePostalFrancaisType x) = simpleTypeText x
 
newtype CommuneFranceCodeInseeType = CommuneFranceCodeInseeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CommuneFranceCodeInseeType Xsd.XsdString where
    restricts (CommuneFranceCodeInseeType x) = x
instance SchemaType CommuneFranceCodeInseeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CommuneFranceCodeInseeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CommuneFranceCodeInseeType where
    acceptingParser = fmap CommuneFranceCodeInseeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9A-Z]{5})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CommuneFranceCodeInseeType x) = simpleTypeText x
 
newtype CompteurIntensiteNominaleCodeType = CompteurIntensiteNominaleCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CompteurIntensiteNominaleCodeType Xsd.XsdString where
    restricts (CompteurIntensiteNominaleCodeType x) = x
instance SchemaType CompteurIntensiteNominaleCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CompteurIntensiteNominaleCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CompteurIntensiteNominaleCodeType where
    acceptingParser = fmap CompteurIntensiteNominaleCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CompteurIntensiteNominaleCodeType x) = simpleTypeText x
 
newtype CompteurModeleSousTypeCodeType = CompteurModeleSousTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CompteurModeleSousTypeCodeType Xsd.XsdString where
    restricts (CompteurModeleSousTypeCodeType x) = x
instance SchemaType CompteurModeleSousTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CompteurModeleSousTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CompteurModeleSousTypeCodeType where
    acceptingParser = fmap CompteurModeleSousTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CompteurModeleSousTypeCodeType x) = simpleTypeText x
 
newtype CompteurTensionNominaleCodeType = CompteurTensionNominaleCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CompteurTensionNominaleCodeType Xsd.XsdString where
    restricts (CompteurTensionNominaleCodeType x) = x
instance SchemaType CompteurTensionNominaleCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CompteurTensionNominaleCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CompteurTensionNominaleCodeType where
    acceptingParser = fmap CompteurTensionNominaleCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CompteurTensionNominaleCodeType x) = simpleTypeText x
 
newtype CompteurTypeCodeType = CompteurTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CompteurTypeCodeType Xsd.XsdString where
    restricts (CompteurTypeCodeType x) = x
instance SchemaType CompteurTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CompteurTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CompteurTypeCodeType where
    acceptingParser = fmap CompteurTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CompteurTypeCodeType x) = simpleTypeText x
 
data ConsommationTypeCodeType
    = ConsommationTypeCodeType_FACTU
    | ConsommationTypeCodeType_MOYEN
    deriving (Eq,Show,Enum)
instance SchemaType ConsommationTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ConsommationTypeCodeType where
    acceptingParser =  do _ <- literal "FACTU"; return ConsommationTypeCodeType_FACTU
                      `onFail` do _ <- literal "MOYEN"; return ConsommationTypeCodeType_MOYEN
                      
    simpleTypeText ConsommationTypeCodeType_FACTU = "FACTU"
    simpleTypeText ConsommationTypeCodeType_MOYEN = "MOYEN"
 
newtype ConsuelMotifNonExigibiliteCodeType = ConsuelMotifNonExigibiliteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ConsuelMotifNonExigibiliteCodeType Xsd.XsdString where
    restricts (ConsuelMotifNonExigibiliteCodeType x) = x
instance SchemaType ConsuelMotifNonExigibiliteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ConsuelMotifNonExigibiliteCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ConsuelMotifNonExigibiliteCodeType where
    acceptingParser = fmap ConsuelMotifNonExigibiliteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ConsuelMotifNonExigibiliteCodeType x) = simpleTypeText x
 
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
 
newtype CoupureLocalisationCodeType = CoupureLocalisationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CoupureLocalisationCodeType Xsd.XsdString where
    restricts (CoupureLocalisationCodeType x) = x
instance SchemaType CoupureLocalisationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CoupureLocalisationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CoupureLocalisationCodeType where
    acceptingParser = fmap CoupureLocalisationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CoupureLocalisationCodeType x) = simpleTypeText x
 
newtype CoupureMotifCodeType = CoupureMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CoupureMotifCodeType Xsd.XsdString where
    restricts (CoupureMotifCodeType x) = x
instance SchemaType CoupureMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CoupureMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CoupureMotifCodeType where
    acceptingParser = fmap CoupureMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CoupureMotifCodeType x) = simpleTypeText x
 
newtype CourrierIdType = CourrierIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts CourrierIdType Xsd.XsdString where
    restricts (CourrierIdType x) = x
instance SchemaType CourrierIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CourrierIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CourrierIdType where
    acceptingParser = fmap CourrierIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CourrierIdType x) = simpleTypeText x
 
newtype CourrierModeleCodeType = CourrierModeleCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CourrierModeleCodeType Xsd.XsdString where
    restricts (CourrierModeleCodeType x) = x
instance SchemaType CourrierModeleCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CourrierModeleCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CourrierModeleCodeType where
    acceptingParser = fmap CourrierModeleCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CourrierModeleCodeType x) = simpleTypeText x
 
newtype CourrierMotifNonDistributionCodeType = CourrierMotifNonDistributionCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CourrierMotifNonDistributionCodeType Xsd.XsdString where
    restricts (CourrierMotifNonDistributionCodeType x) = x
instance SchemaType CourrierMotifNonDistributionCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CourrierMotifNonDistributionCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CourrierMotifNonDistributionCodeType where
    acceptingParser = fmap CourrierMotifNonDistributionCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CourrierMotifNonDistributionCodeType x) = simpleTypeText x
 
newtype CourrierStatutCodeType = CourrierStatutCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CourrierStatutCodeType Xsd.XsdString where
    restricts (CourrierStatutCodeType x) = x
instance SchemaType CourrierStatutCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CourrierStatutCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CourrierStatutCodeType where
    acceptingParser = fmap CourrierStatutCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CourrierStatutCodeType x) = simpleTypeText x
 
newtype CourrierTypeCodeType = CourrierTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CourrierTypeCodeType Xsd.XsdString where
    restricts (CourrierTypeCodeType x) = x
instance SchemaType CourrierTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CourrierTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CourrierTypeCodeType where
    acceptingParser = fmap CourrierTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (CourrierTypeCodeType x) = simpleTypeText x
 
newtype CreneauHoraireCodeType = CreneauHoraireCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts CreneauHoraireCodeType Xsd.XsdString where
    restricts (CreneauHoraireCodeType x) = x
instance SchemaType CreneauHoraireCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (CreneauHoraireCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType CreneauHoraireCodeType where
    acceptingParser = fmap CreneauHoraireCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (CreneauHoraireCodeType x) = simpleTypeText x
 
newtype DateHeureType = DateHeureType Xsd.DateTime deriving (Eq,Show)
instance Restricts DateHeureType Xsd.DateTime where
    restricts (DateHeureType x) = x
instance SchemaType DateHeureType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DateHeureType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DateHeureType where
    acceptingParser = fmap DateHeureType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (DateHeureType x) = simpleTypeText x
 
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
 
newtype DemandeChangementFrnCorrectifMotifCodeType = DemandeChangementFrnCorrectifMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeChangementFrnCorrectifMotifCodeType Xsd.XsdString where
    restricts (DemandeChangementFrnCorrectifMotifCodeType x) = x
instance SchemaType DemandeChangementFrnCorrectifMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeChangementFrnCorrectifMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeChangementFrnCorrectifMotifCodeType where
    acceptingParser = fmap DemandeChangementFrnCorrectifMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{4,10})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (DemandeChangementFrnCorrectifMotifCodeType x) = simpleTypeText x
 
newtype DemandeCommunicationDistributeurTypeCodeType = DemandeCommunicationDistributeurTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeCommunicationDistributeurTypeCodeType Xsd.XsdString where
    restricts (DemandeCommunicationDistributeurTypeCodeType x) = x
instance SchemaType DemandeCommunicationDistributeurTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeCommunicationDistributeurTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeCommunicationDistributeurTypeCodeType where
    acceptingParser = fmap DemandeCommunicationDistributeurTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeCommunicationDistributeurTypeCodeType x) = simpleTypeText x
 
newtype DemandeDiverseComptageTypeCodeType = DemandeDiverseComptageTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeDiverseComptageTypeCodeType Xsd.XsdString where
    restricts (DemandeDiverseComptageTypeCodeType x) = x
instance SchemaType DemandeDiverseComptageTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeDiverseComptageTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeDiverseComptageTypeCodeType where
    acceptingParser = fmap DemandeDiverseComptageTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeDiverseComptageTypeCodeType x) = simpleTypeText x
 
newtype DemandeDiverseInformationsTypeCodeType = DemandeDiverseInformationsTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeDiverseInformationsTypeCodeType Xsd.XsdString where
    restricts (DemandeDiverseInformationsTypeCodeType x) = x
instance SchemaType DemandeDiverseInformationsTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeDiverseInformationsTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeDiverseInformationsTypeCodeType where
    acceptingParser = fmap DemandeDiverseInformationsTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeDiverseInformationsTypeCodeType x) = simpleTypeText x
 
newtype DemandeDiverseQualiteFournitureTypeCodeType = DemandeDiverseQualiteFournitureTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeDiverseQualiteFournitureTypeCodeType Xsd.XsdString where
    restricts (DemandeDiverseQualiteFournitureTypeCodeType x) = x
instance SchemaType DemandeDiverseQualiteFournitureTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeDiverseQualiteFournitureTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeDiverseQualiteFournitureTypeCodeType where
    acceptingParser = fmap DemandeDiverseQualiteFournitureTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeDiverseQualiteFournitureTypeCodeType x) = simpleTypeText x
 
newtype DemandeDiverseReseauTypeCodeType = DemandeDiverseReseauTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeDiverseReseauTypeCodeType Xsd.XsdString where
    restricts (DemandeDiverseReseauTypeCodeType x) = x
instance SchemaType DemandeDiverseReseauTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeDiverseReseauTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeDiverseReseauTypeCodeType where
    acceptingParser = fmap DemandeDiverseReseauTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeDiverseReseauTypeCodeType x) = simpleTypeText x
 
newtype DemandeMediaCodeType = DemandeMediaCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeMediaCodeType Xsd.XsdString where
    restricts (DemandeMediaCodeType x) = x
instance SchemaType DemandeMediaCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeMediaCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeMediaCodeType where
    acceptingParser = fmap DemandeMediaCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (DemandeMediaCodeType x) = simpleTypeText x
 
newtype DemandeMiseEnServiceCorrectifMotifCodeType = DemandeMiseEnServiceCorrectifMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeMiseEnServiceCorrectifMotifCodeType Xsd.XsdString where
    restricts (DemandeMiseEnServiceCorrectifMotifCodeType x) = x
instance SchemaType DemandeMiseEnServiceCorrectifMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeMiseEnServiceCorrectifMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeMiseEnServiceCorrectifMotifCodeType where
    acceptingParser = fmap DemandeMiseEnServiceCorrectifMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (DemandeMiseEnServiceCorrectifMotifCodeType x) = simpleTypeText x
 
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
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (DemandeObjetCodeType x) = simpleTypeText x
 
newtype DemandeTechniqueTypeCodeType = DemandeTechniqueTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DemandeTechniqueTypeCodeType Xsd.XsdString where
    restricts (DemandeTechniqueTypeCodeType x) = x
instance SchemaType DemandeTechniqueTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DemandeTechniqueTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DemandeTechniqueTypeCodeType where
    acceptingParser = fmap DemandeTechniqueTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DemandeTechniqueTypeCodeType x) = simpleTypeText x
 
newtype DeviseAbreviationType = DeviseAbreviationType Xsd.XsdString deriving (Eq,Show)
instance Restricts DeviseAbreviationType Xsd.XsdString where
    restricts (DeviseAbreviationType x) = x
instance SchemaType DeviseAbreviationType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DeviseAbreviationType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DeviseAbreviationType where
    acceptingParser = fmap DeviseAbreviationType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (DeviseAbreviationType x) = simpleTypeText x
 
newtype DisjoncteurCalibreCodeType = DisjoncteurCalibreCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DisjoncteurCalibreCodeType Xsd.XsdString where
    restricts (DisjoncteurCalibreCodeType x) = x
instance SchemaType DisjoncteurCalibreCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DisjoncteurCalibreCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DisjoncteurCalibreCodeType where
    acceptingParser = fmap DisjoncteurCalibreCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DisjoncteurCalibreCodeType x) = simpleTypeText x
 
newtype DisjoncteurNatureCodeType = DisjoncteurNatureCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DisjoncteurNatureCodeType Xsd.XsdString where
    restricts (DisjoncteurNatureCodeType x) = x
instance SchemaType DisjoncteurNatureCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DisjoncteurNatureCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DisjoncteurNatureCodeType where
    acceptingParser = fmap DisjoncteurNatureCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DisjoncteurNatureCodeType x) = simpleTypeText x
 
newtype DispositifComptageParticulariteCodeType = DispositifComptageParticulariteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DispositifComptageParticulariteCodeType Xsd.XsdString where
    restricts (DispositifComptageParticulariteCodeType x) = x
instance SchemaType DispositifComptageParticulariteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DispositifComptageParticulariteCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DispositifComptageParticulariteCodeType where
    acceptingParser = fmap DispositifComptageParticulariteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DispositifComptageParticulariteCodeType x) = simpleTypeText x
 
newtype DispositifParticulierLimitationPerturbationsCodeType = DispositifParticulierLimitationPerturbationsCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DispositifParticulierLimitationPerturbationsCodeType Xsd.XsdString where
    restricts (DispositifParticulierLimitationPerturbationsCodeType x) = x
instance SchemaType DispositifParticulierLimitationPerturbationsCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DispositifParticulierLimitationPerturbationsCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DispositifParticulierLimitationPerturbationsCodeType where
    acceptingParser = fmap DispositifParticulierLimitationPerturbationsCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DispositifParticulierLimitationPerturbationsCodeType x) = simpleTypeText x
 
data DomaineTensionCodeType
    = DomaineTensionCodeType_BTINF
    | DomaineTensionCodeType_BTSUP
    | DomaineTensionCodeType_HTA
    | DomaineTensionCodeType_HTB
    deriving (Eq,Show,Enum)
instance SchemaType DomaineTensionCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DomaineTensionCodeType where
    acceptingParser =  do _ <- literal "BTINF"; return DomaineTensionCodeType_BTINF
                      `onFail` do _ <- literal "BTSUP"; return DomaineTensionCodeType_BTSUP
                      `onFail` do _ <- literal "HTA"; return DomaineTensionCodeType_HTA
                      `onFail` do _ <- literal "HTB"; return DomaineTensionCodeType_HTB
                      
    simpleTypeText DomaineTensionCodeType_BTINF = "BTINF"
    simpleTypeText DomaineTensionCodeType_BTSUP = "BTSUP"
    simpleTypeText DomaineTensionCodeType_HTA = "HTA"
    simpleTypeText DomaineTensionCodeType_HTB = "HTB"
 
data DureeUniteSymboleType
    = DureeUniteSymboleType_Annee
    | DureeUniteSymboleType_H
    | DureeUniteSymboleType_Jour
    | DureeUniteSymboleType_Min
    | DureeUniteSymboleType_Mois
    | DureeUniteSymboleType_Ms
    | DureeUniteSymboleType_S
    | DureeUniteSymboleType_Semaine
    deriving (Eq,Show,Enum)
instance SchemaType DureeUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DureeUniteSymboleType where
    acceptingParser =  do _ <- literal "annee"; return DureeUniteSymboleType_Annee
                      `onFail` do _ <- literal "h"; return DureeUniteSymboleType_H
                      `onFail` do _ <- literal "jour"; return DureeUniteSymboleType_Jour
                      `onFail` do _ <- literal "min"; return DureeUniteSymboleType_Min
                      `onFail` do _ <- literal "mois"; return DureeUniteSymboleType_Mois
                      `onFail` do _ <- literal "ms"; return DureeUniteSymboleType_Ms
                      `onFail` do _ <- literal "s"; return DureeUniteSymboleType_S
                      `onFail` do _ <- literal "semaine"; return DureeUniteSymboleType_Semaine
                      
    simpleTypeText DureeUniteSymboleType_Annee = "annee"
    simpleTypeText DureeUniteSymboleType_H = "h"
    simpleTypeText DureeUniteSymboleType_Jour = "jour"
    simpleTypeText DureeUniteSymboleType_Min = "min"
    simpleTypeText DureeUniteSymboleType_Mois = "mois"
    simpleTypeText DureeUniteSymboleType_Ms = "ms"
    simpleTypeText DureeUniteSymboleType_S = "s"
    simpleTypeText DureeUniteSymboleType_Semaine = "semaine"
 
newtype DysfonctionnementNatureCodeType = DysfonctionnementNatureCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts DysfonctionnementNatureCodeType Xsd.XsdString where
    restricts (DysfonctionnementNatureCodeType x) = x
instance SchemaType DysfonctionnementNatureCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (DysfonctionnementNatureCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType DysfonctionnementNatureCodeType where
    acceptingParser = fmap DysfonctionnementNatureCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (DysfonctionnementNatureCodeType x) = simpleTypeText x
 
newtype EquipementElectriqueLocalisationCodeType = EquipementElectriqueLocalisationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts EquipementElectriqueLocalisationCodeType Xsd.XsdString where
    restricts (EquipementElectriqueLocalisationCodeType x) = x
instance SchemaType EquipementElectriqueLocalisationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (EquipementElectriqueLocalisationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EquipementElectriqueLocalisationCodeType where
    acceptingParser = fmap EquipementElectriqueLocalisationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (EquipementElectriqueLocalisationCodeType x) = simpleTypeText x
 
newtype EquipementElectriqueRegimeProprieteCodeType = EquipementElectriqueRegimeProprieteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts EquipementElectriqueRegimeProprieteCodeType Xsd.XsdString where
    restricts (EquipementElectriqueRegimeProprieteCodeType x) = x
instance SchemaType EquipementElectriqueRegimeProprieteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (EquipementElectriqueRegimeProprieteCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EquipementElectriqueRegimeProprieteCodeType where
    acceptingParser = fmap EquipementElectriqueRegimeProprieteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (EquipementElectriqueRegimeProprieteCodeType x) = simpleTypeText x
 
newtype EtablissementNumSiretType = EtablissementNumSiretType Xsd.XsdString deriving (Eq,Show)
instance Restricts EtablissementNumSiretType Xsd.XsdString where
    restricts (EtablissementNumSiretType x) = x
instance SchemaType EtablissementNumSiretType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (EtablissementNumSiretType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType EtablissementNumSiretType where
    acceptingParser = fmap EtablissementNumSiretType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{14})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (EtablissementNumSiretType x) = simpleTypeText x
 
newtype FaisabiliteMotifImpossibiliteCodeType = FaisabiliteMotifImpossibiliteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FaisabiliteMotifImpossibiliteCodeType Xsd.XsdString where
    restricts (FaisabiliteMotifImpossibiliteCodeType x) = x
instance SchemaType FaisabiliteMotifImpossibiliteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FaisabiliteMotifImpossibiliteCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FaisabiliteMotifImpossibiliteCodeType where
    acceptingParser = fmap FaisabiliteMotifImpossibiliteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FaisabiliteMotifImpossibiliteCodeType x) = simpleTypeText x
 
newtype FaisabiliteReserveMotifCodeType = FaisabiliteReserveMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FaisabiliteReserveMotifCodeType Xsd.XsdString where
    restricts (FaisabiliteReserveMotifCodeType x) = x
instance SchemaType FaisabiliteReserveMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FaisabiliteReserveMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FaisabiliteReserveMotifCodeType where
    acceptingParser = fmap FaisabiliteReserveMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FaisabiliteReserveMotifCodeType x) = simpleTypeText x
 
newtype FichierIdType = FichierIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts FichierIdType Xsd.XsdString where
    restricts (FichierIdType x) = x
instance SchemaType FichierIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FichierIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FichierIdType where
    acceptingParser = fmap FichierIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FichierIdType x) = simpleTypeText x
 
newtype FichierNomType = FichierNomType Xsd.XsdString deriving (Eq,Show)
instance Restricts FichierNomType Xsd.XsdString where
    restricts (FichierNomType x) = x
instance SchemaType FichierNomType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FichierNomType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FichierNomType where
    acceptingParser = fmap FichierNomType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern \S{1,120})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (FichierNomType x) = simpleTypeText x
 
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
 
newtype FraisCodeType = FraisCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FraisCodeType Xsd.XsdString where
    restricts (FraisCodeType x) = x
instance SchemaType FraisCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FraisCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FraisCodeType where
    acceptingParser = fmap FraisCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FraisCodeType x) = simpleTypeText x
 
newtype FraudeNatureCodeType = FraudeNatureCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FraudeNatureCodeType Xsd.XsdString where
    restricts (FraudeNatureCodeType x) = x
instance SchemaType FraudeNatureCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FraudeNatureCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FraudeNatureCodeType where
    acceptingParser = fmap FraudeNatureCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FraudeNatureCodeType x) = simpleTypeText x
 
newtype FraudeOuDysfonctionnementMethodeCalculCodeType = FraudeOuDysfonctionnementMethodeCalculCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FraudeOuDysfonctionnementMethodeCalculCodeType Xsd.XsdString where
    restricts (FraudeOuDysfonctionnementMethodeCalculCodeType x) = x
instance SchemaType FraudeOuDysfonctionnementMethodeCalculCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FraudeOuDysfonctionnementMethodeCalculCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FraudeOuDysfonctionnementMethodeCalculCodeType where
    acceptingParser = fmap FraudeOuDysfonctionnementMethodeCalculCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FraudeOuDysfonctionnementMethodeCalculCodeType x) = simpleTypeText x
 
newtype FraudeOuDysfonctionnementMethodeFacturationCodeType = FraudeOuDysfonctionnementMethodeFacturationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts FraudeOuDysfonctionnementMethodeFacturationCodeType Xsd.XsdString where
    restricts (FraudeOuDysfonctionnementMethodeFacturationCodeType x) = x
instance SchemaType FraudeOuDysfonctionnementMethodeFacturationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (FraudeOuDysfonctionnementMethodeFacturationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType FraudeOuDysfonctionnementMethodeFacturationCodeType where
    acceptingParser = fmap FraudeOuDysfonctionnementMethodeFacturationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (FraudeOuDysfonctionnementMethodeFacturationCodeType x) = simpleTypeText x
 
newtype GrandeurMetierCodeType = GrandeurMetierCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts GrandeurMetierCodeType Xsd.XsdString where
    restricts (GrandeurMetierCodeType x) = x
instance SchemaType GrandeurMetierCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (GrandeurMetierCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GrandeurMetierCodeType where
    acceptingParser = fmap GrandeurMetierCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (GrandeurMetierCodeType x) = simpleTypeText x
 
newtype GrandeurPhysiqueCodeType = GrandeurPhysiqueCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts GrandeurPhysiqueCodeType Xsd.XsdString where
    restricts (GrandeurPhysiqueCodeType x) = x
instance SchemaType GrandeurPhysiqueCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (GrandeurPhysiqueCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GrandeurPhysiqueCodeType where
    acceptingParser = fmap GrandeurPhysiqueCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GrandeurPhysiqueCodeType x) = simpleTypeText x
 
newtype GroupePeriodeMobileCodeType = GroupePeriodeMobileCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts GroupePeriodeMobileCodeType Xsd.XsdString where
    restricts (GroupePeriodeMobileCodeType x) = x
instance SchemaType GroupePeriodeMobileCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (GroupePeriodeMobileCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType GroupePeriodeMobileCodeType where
    acceptingParser = fmap GroupePeriodeMobileCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (GroupePeriodeMobileCodeType x) = simpleTypeText x
 
newtype HeureType = HeureType Xsd.Time deriving (Eq,Show)
instance Restricts HeureType Xsd.Time where
    restricts (HeureType x) = x
instance SchemaType HeureType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (HeureType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType HeureType where
    acceptingParser = fmap HeureType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (HeureType x) = simpleTypeText x
 
newtype IndexUniteSymboleType = IndexUniteSymboleType Xsd.XsdString deriving (Eq,Show)
instance Restricts IndexUniteSymboleType Xsd.XsdString where
    restricts (IndexUniteSymboleType x) = x
instance SchemaType IndexUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (IndexUniteSymboleType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType IndexUniteSymboleType where
    acceptingParser = fmap IndexUniteSymboleType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [a-zA-Z]{1,5})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (IndexUniteSymboleType x) = simpleTypeText x
 
data IntensiteUniteSymboleType
    = IntensiteUniteSymboleType_A
    | IntensiteUniteSymboleType_MA
    deriving (Eq,Show,Enum)
instance SchemaType IntensiteUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType IntensiteUniteSymboleType where
    acceptingParser =  do _ <- literal "A"; return IntensiteUniteSymboleType_A
                      `onFail` do _ <- literal "mA"; return IntensiteUniteSymboleType_MA
                      
    simpleTypeText IntensiteUniteSymboleType_A = "A"
    simpleTypeText IntensiteUniteSymboleType_MA = "mA"
 
newtype IntervenantTypeCodeType = IntervenantTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts IntervenantTypeCodeType Xsd.XsdString where
    restricts (IntervenantTypeCodeType x) = x
instance SchemaType IntervenantTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (IntervenantTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType IntervenantTypeCodeType where
    acceptingParser = fmap IntervenantTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (IntervenantTypeCodeType x) = simpleTypeText x
 
newtype InterventionDemandeMotifRefusCodeType = InterventionDemandeMotifRefusCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts InterventionDemandeMotifRefusCodeType Xsd.XsdString where
    restricts (InterventionDemandeMotifRefusCodeType x) = x
instance SchemaType InterventionDemandeMotifRefusCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (InterventionDemandeMotifRefusCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionDemandeMotifRefusCodeType where
    acceptingParser = fmap InterventionDemandeMotifRefusCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (InterventionDemandeMotifRefusCodeType x) = simpleTypeText x
 
newtype InterventionDeplanificationMotifCodeType = InterventionDeplanificationMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts InterventionDeplanificationMotifCodeType Xsd.XsdString where
    restricts (InterventionDeplanificationMotifCodeType x) = x
instance SchemaType InterventionDeplanificationMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (InterventionDeplanificationMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionDeplanificationMotifCodeType where
    acceptingParser = fmap InterventionDeplanificationMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (InterventionDeplanificationMotifCodeType x) = simpleTypeText x
 
data InterventionEtatCodeType
    = InterventionEtatCodeType_ANNUL
    | InterventionEtatCodeType_CLOS
    | InterventionEtatCodeType_DMND
    | InterventionEtatCodeType_PLAN
    deriving (Eq,Show,Enum)
instance SchemaType InterventionEtatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionEtatCodeType where
    acceptingParser =  do _ <- literal "ANNUL"; return InterventionEtatCodeType_ANNUL
                      `onFail` do _ <- literal "CLOS"; return InterventionEtatCodeType_CLOS
                      `onFail` do _ <- literal "DMND"; return InterventionEtatCodeType_DMND
                      `onFail` do _ <- literal "PLAN"; return InterventionEtatCodeType_PLAN
                      
    simpleTypeText InterventionEtatCodeType_ANNUL = "ANNUL"
    simpleTypeText InterventionEtatCodeType_CLOS = "CLOS"
    simpleTypeText InterventionEtatCodeType_DMND = "DMND"
    simpleTypeText InterventionEtatCodeType_PLAN = "PLAN"
 
newtype InterventionNonRealisationMotifCodeType = InterventionNonRealisationMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts InterventionNonRealisationMotifCodeType Xsd.XsdString where
    restricts (InterventionNonRealisationMotifCodeType x) = x
instance SchemaType InterventionNonRealisationMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (InterventionNonRealisationMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionNonRealisationMotifCodeType where
    acceptingParser = fmap InterventionNonRealisationMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (InterventionNonRealisationMotifCodeType x) = simpleTypeText x
 
data InterventionPeriodeTypeCodeType
    = InterventionPeriodeTypeCodeType_HEOE
    | InterventionPeriodeTypeCodeType_HHOE
    | InterventionPeriodeTypeCodeType_HHOS
    | InterventionPeriodeTypeCodeType_HHOW
    | InterventionPeriodeTypeCodeType_HHSW
    deriving (Eq,Show,Enum)
instance SchemaType InterventionPeriodeTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionPeriodeTypeCodeType where
    acceptingParser =  do _ <- literal "HEOE"; return InterventionPeriodeTypeCodeType_HEOE
                      `onFail` do _ <- literal "HHOE"; return InterventionPeriodeTypeCodeType_HHOE
                      `onFail` do _ <- literal "HHOS"; return InterventionPeriodeTypeCodeType_HHOS
                      `onFail` do _ <- literal "HHOW"; return InterventionPeriodeTypeCodeType_HHOW
                      `onFail` do _ <- literal "HHSW"; return InterventionPeriodeTypeCodeType_HHSW
                      
    simpleTypeText InterventionPeriodeTypeCodeType_HEOE = "HEOE"
    simpleTypeText InterventionPeriodeTypeCodeType_HHOE = "HHOE"
    simpleTypeText InterventionPeriodeTypeCodeType_HHOS = "HHOS"
    simpleTypeText InterventionPeriodeTypeCodeType_HHOW = "HHOW"
    simpleTypeText InterventionPeriodeTypeCodeType_HHSW = "HHSW"
 
newtype InterventionPlanificationHorsDelaiMotifCodeType = InterventionPlanificationHorsDelaiMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts InterventionPlanificationHorsDelaiMotifCodeType Xsd.XsdString where
    restricts (InterventionPlanificationHorsDelaiMotifCodeType x) = x
instance SchemaType InterventionPlanificationHorsDelaiMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (InterventionPlanificationHorsDelaiMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionPlanificationHorsDelaiMotifCodeType where
    acceptingParser = fmap InterventionPlanificationHorsDelaiMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (InterventionPlanificationHorsDelaiMotifCodeType x) = simpleTypeText x
 
data InterventionRealisationEtatCodeType
    = InterventionRealisationEtatCodeType_NREA
    | InterventionRealisationEtatCodeType_PREA
    | InterventionRealisationEtatCodeType_REAL
    deriving (Eq,Show,Enum)
instance SchemaType InterventionRealisationEtatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionRealisationEtatCodeType where
    acceptingParser =  do _ <- literal "NREA"; return InterventionRealisationEtatCodeType_NREA
                      `onFail` do _ <- literal "PREA"; return InterventionRealisationEtatCodeType_PREA
                      `onFail` do _ <- literal "REAL"; return InterventionRealisationEtatCodeType_REAL
                      
    simpleTypeText InterventionRealisationEtatCodeType_NREA = "NREA"
    simpleTypeText InterventionRealisationEtatCodeType_PREA = "PREA"
    simpleTypeText InterventionRealisationEtatCodeType_REAL = "REAL"
 
newtype InterventionReplanificationMotifCodeType = InterventionReplanificationMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts InterventionReplanificationMotifCodeType Xsd.XsdString where
    restricts (InterventionReplanificationMotifCodeType x) = x
instance SchemaType InterventionReplanificationMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (InterventionReplanificationMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType InterventionReplanificationMotifCodeType where
    acceptingParser = fmap InterventionReplanificationMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (InterventionReplanificationMotifCodeType x) = simpleTypeText x
 
newtype JourDuMoisType = JourDuMoisType Xsd.GDay deriving (Eq,Show)
instance Restricts JourDuMoisType Xsd.GDay where
    restricts (JourDuMoisType x) = x
instance SchemaType JourDuMoisType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (JourDuMoisType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType JourDuMoisType where
    acceptingParser = fmap JourDuMoisType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (JourDuMoisType x) = simpleTypeText x
 
newtype LibelleAffichageCompteurType = LibelleAffichageCompteurType Xsd.XsdString deriving (Eq,Show)
instance Restricts LibelleAffichageCompteurType Xsd.XsdString where
    restricts (LibelleAffichageCompteurType x) = x
instance SchemaType LibelleAffichageCompteurType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (LibelleAffichageCompteurType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LibelleAffichageCompteurType where
    acceptingParser = fmap LibelleAffichageCompteurType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Za-z0-9 _\-]{1,16})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (LibelleAffichageCompteurType x) = simpleTypeText x
 
newtype LigneTelephoniqueNumType = LigneTelephoniqueNumType Xsd.XsdString deriving (Eq,Show)
instance Restricts LigneTelephoniqueNumType Xsd.XsdString where
    restricts (LigneTelephoniqueNumType x) = x
instance SchemaType LigneTelephoniqueNumType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (LigneTelephoniqueNumType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LigneTelephoniqueNumType where
    acceptingParser = fmap LigneTelephoniqueNumType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9\+\(\)\s\.]{1,25})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (LigneTelephoniqueNumType x) = simpleTypeText x
 
newtype LimiteurTypeCodeType = LimiteurTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts LimiteurTypeCodeType Xsd.XsdString where
    restricts (LimiteurTypeCodeType x) = x
instance SchemaType LimiteurTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (LimiteurTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LimiteurTypeCodeType where
    acceptingParser = fmap LimiteurTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (LimiteurTypeCodeType x) = simpleTypeText x
 
data LongueurUniteSymboleType
    = LongueurUniteSymboleType_Km
    | LongueurUniteSymboleType_M
    deriving (Eq,Show,Enum)
instance SchemaType LongueurUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LongueurUniteSymboleType where
    acceptingParser =  do _ <- literal "km"; return LongueurUniteSymboleType_Km
                      `onFail` do _ <- literal "m"; return LongueurUniteSymboleType_M
                      
    simpleTypeText LongueurUniteSymboleType_Km = "km"
    simpleTypeText LongueurUniteSymboleType_M = "m"
 
newtype MesureDeclencheurCodeType = MesureDeclencheurCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts MesureDeclencheurCodeType Xsd.XsdString where
    restricts (MesureDeclencheurCodeType x) = x
instance SchemaType MesureDeclencheurCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesureDeclencheurCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesureDeclencheurCodeType where
    acceptingParser = fmap MesureDeclencheurCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (MesureDeclencheurCodeType x) = simpleTypeText x
 
newtype MesureNatureCodeType = MesureNatureCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts MesureNatureCodeType Xsd.XsdString where
    restricts (MesureNatureCodeType x) = x
instance SchemaType MesureNatureCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesureNatureCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesureNatureCodeType where
    acceptingParser = fmap MesureNatureCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (MesureNatureCodeType x) = simpleTypeText x
 
newtype MesureOrigineCodeType = MesureOrigineCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts MesureOrigineCodeType Xsd.XsdString where
    restricts (MesureOrigineCodeType x) = x
instance SchemaType MesureOrigineCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesureOrigineCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesureOrigineCodeType where
    acceptingParser = fmap MesureOrigineCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (MesureOrigineCodeType x) = simpleTypeText x
 
newtype MesureStatutCodeType = MesureStatutCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts MesureStatutCodeType Xsd.XsdString where
    restricts (MesureStatutCodeType x) = x
instance SchemaType MesureStatutCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesureStatutCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesureStatutCodeType where
    acceptingParser = fmap MesureStatutCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (MesureStatutCodeType x) = simpleTypeText x
 
newtype MesureTypeCodeType = MesureTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts MesureTypeCodeType Xsd.XsdString where
    restricts (MesureTypeCodeType x) = x
instance SchemaType MesureTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesureTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesureTypeCodeType where
    acceptingParser = fmap MesureTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{3,10})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (MesureTypeCodeType x) = simpleTypeText x
 
newtype MesureUniteSymboleType = MesureUniteSymboleType Xsd.XsdString deriving (Eq,Show)
instance Restricts MesureUniteSymboleType Xsd.XsdString where
    restricts (MesureUniteSymboleType x) = x
instance SchemaType MesureUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MesureUniteSymboleType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MesureUniteSymboleType where
    acceptingParser = fmap MesureUniteSymboleType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (MesureUniteSymboleType x) = simpleTypeText x
 
newtype MoisEtJourType = MoisEtJourType Xsd.GMonthDay deriving (Eq,Show)
instance Restricts MoisEtJourType Xsd.GMonthDay where
    restricts (MoisEtJourType x) = x
instance SchemaType MoisEtJourType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MoisEtJourType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MoisEtJourType where
    acceptingParser = fmap MoisEtJourType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (MoisEtJourType x) = simpleTypeText x
 
newtype MoisType = MoisType Xsd.GMonth deriving (Eq,Show)
instance Restricts MoisType Xsd.GMonth where
    restricts (MoisType x) = x
instance SchemaType MoisType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (MoisType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType MoisType where
    acceptingParser = fmap MoisType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (MoisType x) = simpleTypeText x
 
newtype NbDecimalType = NbDecimalType Xsd.Decimal deriving (Eq,Show)
instance Restricts NbDecimalType Xsd.Decimal where
    restricts (NbDecimalType x) = x
instance SchemaType NbDecimalType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (NbDecimalType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NbDecimalType where
    acceptingParser = fmap NbDecimalType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (NbDecimalType x) = simpleTypeText x
 
newtype NbEntierStrictementPositifType = NbEntierStrictementPositifType Xsd.PositiveInteger deriving (Eq,Show)
instance Restricts NbEntierStrictementPositifType Xsd.PositiveInteger where
    restricts (NbEntierStrictementPositifType x) = x
instance SchemaType NbEntierStrictementPositifType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (NbEntierStrictementPositifType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NbEntierStrictementPositifType where
    acceptingParser = fmap NbEntierStrictementPositifType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (NbEntierStrictementPositifType x) = simpleTypeText x
 
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
 
newtype NiveauOuvertureServicesCodeType = NiveauOuvertureServicesCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts NiveauOuvertureServicesCodeType Xsd.XsdString where
    restricts (NiveauOuvertureServicesCodeType x) = x
instance SchemaType NiveauOuvertureServicesCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (NiveauOuvertureServicesCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NiveauOuvertureServicesCodeType where
    acceptingParser = fmap NiveauOuvertureServicesCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{1})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (NiveauOuvertureServicesCodeType x) = simpleTypeText x
 
newtype NonPriseEnCompteAutoreleveMotifCodeType = NonPriseEnCompteAutoreleveMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts NonPriseEnCompteAutoreleveMotifCodeType Xsd.XsdString where
    restricts (NonPriseEnCompteAutoreleveMotifCodeType x) = x
instance SchemaType NonPriseEnCompteAutoreleveMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (NonPriseEnCompteAutoreleveMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NonPriseEnCompteAutoreleveMotifCodeType where
    acceptingParser = fmap NonPriseEnCompteAutoreleveMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (NonPriseEnCompteAutoreleveMotifCodeType x) = simpleTypeText x
 
data OffreTypeCodeType
    = OffreTypeCodeType_NO
    | OffreTypeCodeType_OH
    deriving (Eq,Show,Enum)
instance SchemaType OffreTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType OffreTypeCodeType where
    acceptingParser =  do _ <- literal "NO"; return OffreTypeCodeType_NO
                      `onFail` do _ <- literal "OH"; return OffreTypeCodeType_OH
                      
    simpleTypeText OffreTypeCodeType_NO = "NO"
    simpleTypeText OffreTypeCodeType_OH = "OH"
 
newtype OperationCodeType = OperationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts OperationCodeType Xsd.XsdString where
    restricts (OperationCodeType x) = x
instance SchemaType OperationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (OperationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType OperationCodeType where
    acceptingParser = fmap OperationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (OperationCodeType x) = simpleTypeText x
 
newtype OptionBilanContinuiteFournitureCoupureTypeCodeType = OptionBilanContinuiteFournitureCoupureTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts OptionBilanContinuiteFournitureCoupureTypeCodeType Xsd.XsdString where
    restricts (OptionBilanContinuiteFournitureCoupureTypeCodeType x) = x
instance SchemaType OptionBilanContinuiteFournitureCoupureTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (OptionBilanContinuiteFournitureCoupureTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType OptionBilanContinuiteFournitureCoupureTypeCodeType where
    acceptingParser = fmap OptionBilanContinuiteFournitureCoupureTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (OptionBilanContinuiteFournitureCoupureTypeCodeType x) = simpleTypeText x
 
newtype OptionBilanContinuiteFournitureTypeCodeType = OptionBilanContinuiteFournitureTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts OptionBilanContinuiteFournitureTypeCodeType Xsd.XsdString where
    restricts (OptionBilanContinuiteFournitureTypeCodeType x) = x
instance SchemaType OptionBilanContinuiteFournitureTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (OptionBilanContinuiteFournitureTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType OptionBilanContinuiteFournitureTypeCodeType where
    acceptingParser = fmap OptionBilanContinuiteFournitureTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (OptionBilanContinuiteFournitureTypeCodeType x) = simpleTypeText x
 
newtype OptionContractuelleSouscriteIdType = OptionContractuelleSouscriteIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts OptionContractuelleSouscriteIdType Xsd.XsdString where
    restricts (OptionContractuelleSouscriteIdType x) = x
instance SchemaType OptionContractuelleSouscriteIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (OptionContractuelleSouscriteIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType OptionContractuelleSouscriteIdType where
    acceptingParser = fmap OptionContractuelleSouscriteIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (OptionContractuelleSouscriteIdType x) = simpleTypeText x
 
newtype PeriodiciteCodeType = PeriodiciteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PeriodiciteCodeType Xsd.XsdString where
    restricts (PeriodiciteCodeType x) = x
instance SchemaType PeriodiciteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PeriodiciteCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PeriodiciteCodeType where
    acceptingParser = fmap PeriodiciteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PeriodiciteCodeType x) = simpleTypeText x
 
newtype PersonneMoraleNumSirenType = PersonneMoraleNumSirenType Xsd.XsdString deriving (Eq,Show)
instance Restricts PersonneMoraleNumSirenType Xsd.XsdString where
    restricts (PersonneMoraleNumSirenType x) = x
instance SchemaType PersonneMoraleNumSirenType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PersonneMoraleNumSirenType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PersonneMoraleNumSirenType where
    acceptingParser = fmap PersonneMoraleNumSirenType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9]{9})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (PersonneMoraleNumSirenType x) = simpleTypeText x
 
newtype PlageHeuresCreusesCodeType = PlageHeuresCreusesCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PlageHeuresCreusesCodeType Xsd.XsdString where
    restricts (PlageHeuresCreusesCodeType x) = x
instance SchemaType PlageHeuresCreusesCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PlageHeuresCreusesCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PlageHeuresCreusesCodeType where
    acceptingParser = fmap PlageHeuresCreusesCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PlageHeuresCreusesCodeType x) = simpleTypeText x
 
data PointAppartenanceRegroupementHebergeurDecomptantCodeType
    = PointAppartenanceRegroupementHebergeurDecomptantCodeType_DECO
    | PointAppartenanceRegroupementHebergeurDecomptantCodeType_HEBE
    | PointAppartenanceRegroupementHebergeurDecomptantCodeType_NON
    deriving (Eq,Show,Enum)
instance SchemaType PointAppartenanceRegroupementHebergeurDecomptantCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PointAppartenanceRegroupementHebergeurDecomptantCodeType where
    acceptingParser =  do _ <- literal "DECO"; return PointAppartenanceRegroupementHebergeurDecomptantCodeType_DECO
                      `onFail` do _ <- literal "HEBE"; return PointAppartenanceRegroupementHebergeurDecomptantCodeType_HEBE
                      `onFail` do _ <- literal "NON"; return PointAppartenanceRegroupementHebergeurDecomptantCodeType_NON
                      
    simpleTypeText PointAppartenanceRegroupementHebergeurDecomptantCodeType_DECO = "DECO"
    simpleTypeText PointAppartenanceRegroupementHebergeurDecomptantCodeType_HEBE = "HEBE"
    simpleTypeText PointAppartenanceRegroupementHebergeurDecomptantCodeType_NON = "NON"
 
data PointAppartenanceRegroupementTurpeCodeType
    = PointAppartenanceRegroupementTurpeCodeType_NON
    | PointAppartenanceRegroupementTurpeCodeType_REGT
    | PointAppartenanceRegroupementTurpeCodeType_RGPT
    deriving (Eq,Show,Enum)
instance SchemaType PointAppartenanceRegroupementTurpeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PointAppartenanceRegroupementTurpeCodeType where
    acceptingParser =  do _ <- literal "NON"; return PointAppartenanceRegroupementTurpeCodeType_NON
                      `onFail` do _ <- literal "REGT"; return PointAppartenanceRegroupementTurpeCodeType_REGT
                      `onFail` do _ <- literal "RGPT"; return PointAppartenanceRegroupementTurpeCodeType_RGPT
                      
    simpleTypeText PointAppartenanceRegroupementTurpeCodeType_NON = "NON"
    simpleTypeText PointAppartenanceRegroupementTurpeCodeType_REGT = "REGT"
    simpleTypeText PointAppartenanceRegroupementTurpeCodeType_RGPT = "RGPT"
 
data PointEtatContractuelCodeType
    = PointEtatContractuelCodeType_ECRAC
    | PointEtatContractuelCodeType_INACCE
    | PointEtatContractuelCodeType_ECRES
    | PointEtatContractuelCodeType_IMPRO
    | PointEtatContractuelCodeType_RESIL
    | PointEtatContractuelCodeType_SERVC
    deriving (Eq,Show,Enum)
instance SchemaType PointEtatContractuelCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PointEtatContractuelCodeType where
    acceptingParser =  do _ <- literal "ECRAC"; return PointEtatContractuelCodeType_ECRAC
                      `onFail` do _ <- literal "INACCE"; return PointEtatContractuelCodeType_INACCE
                      `onFail` do _ <- literal "ECRES"; return PointEtatContractuelCodeType_ECRES
                      `onFail` do _ <- literal "IMPRO"; return PointEtatContractuelCodeType_IMPRO
                      `onFail` do _ <- literal "RESIL"; return PointEtatContractuelCodeType_RESIL
                      `onFail` do _ <- literal "SERVC"; return PointEtatContractuelCodeType_SERVC
                      
    simpleTypeText PointEtatContractuelCodeType_ECRAC = "ECRAC"
    simpleTypeText PointEtatContractuelCodeType_INACCE = "INACCE"
    simpleTypeText PointEtatContractuelCodeType_ECRES = "ECRES"
    simpleTypeText PointEtatContractuelCodeType_IMPRO = "IMPRO"
    simpleTypeText PointEtatContractuelCodeType_RESIL = "RESIL"
    simpleTypeText PointEtatContractuelCodeType_SERVC = "SERVC"
 
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
 
data PointSegmentClienteleCodeType
    = PointSegmentClienteleCodeType_C2
    | PointSegmentClienteleCodeType_C3
    | PointSegmentClienteleCodeType_C4
    | PointSegmentClienteleCodeType_C5
    | PointSegmentClienteleCodeType_INDET
    deriving (Eq,Show,Enum)
instance SchemaType PointSegmentClienteleCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PointSegmentClienteleCodeType where
    acceptingParser =  do _ <- literal "C2"; return PointSegmentClienteleCodeType_C2
                      `onFail` do _ <- literal "C3"; return PointSegmentClienteleCodeType_C3
                      `onFail` do _ <- literal "C4"; return PointSegmentClienteleCodeType_C4
                      `onFail` do _ <- literal "C5"; return PointSegmentClienteleCodeType_C5
                      `onFail` do _ <- literal "INDET"; return PointSegmentClienteleCodeType_INDET
                      
    simpleTypeText PointSegmentClienteleCodeType_C2 = "C2"
    simpleTypeText PointSegmentClienteleCodeType_C3 = "C3"
    simpleTypeText PointSegmentClienteleCodeType_C4 = "C4"
    simpleTypeText PointSegmentClienteleCodeType_C5 = "C5"
    simpleTypeText PointSegmentClienteleCodeType_INDET = "INDET"
 
newtype PosteHoraireCodeType = PosteHoraireCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PosteHoraireCodeType Xsd.XsdString where
    restricts (PosteHoraireCodeType x) = x
instance SchemaType PosteHoraireCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PosteHoraireCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PosteHoraireCodeType where
    acceptingParser = fmap PosteHoraireCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PosteHoraireCodeType x) = simpleTypeText x
 
newtype PrejudiceNatureCodeType = PrejudiceNatureCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PrejudiceNatureCodeType Xsd.XsdString where
    restricts (PrejudiceNatureCodeType x) = x
instance SchemaType PrejudiceNatureCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PrejudiceNatureCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrejudiceNatureCodeType where
    acceptingParser = fmap PrejudiceNatureCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PrejudiceNatureCodeType x) = simpleTypeText x
 
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
 
data PrestationEnvisageeCodeType
    = PrestationEnvisageeCodeType_CFN
    | PrestationEnvisageeCodeType_MES
    deriving (Eq,Show,Enum)
instance SchemaType PrestationEnvisageeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrestationEnvisageeCodeType where
    acceptingParser =  do _ <- literal "CFN"; return PrestationEnvisageeCodeType_CFN
                      `onFail` do _ <- literal "MES"; return PrestationEnvisageeCodeType_MES
                      
    simpleTypeText PrestationEnvisageeCodeType_CFN = "CFN"
    simpleTypeText PrestationEnvisageeCodeType_MES = "MES"
 
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
 
newtype PrestationMotifNonFacturationCodeType = PrestationMotifNonFacturationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PrestationMotifNonFacturationCodeType Xsd.XsdString where
    restricts (PrestationMotifNonFacturationCodeType x) = x
instance SchemaType PrestationMotifNonFacturationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PrestationMotifNonFacturationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrestationMotifNonFacturationCodeType where
    acceptingParser = fmap PrestationMotifNonFacturationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PrestationMotifNonFacturationCodeType x) = simpleTypeText x
 
newtype PrestationMotifNonRealisationCodeType = PrestationMotifNonRealisationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts PrestationMotifNonRealisationCodeType Xsd.XsdString where
    restricts (PrestationMotifNonRealisationCodeType x) = x
instance SchemaType PrestationMotifNonRealisationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (PrestationMotifNonRealisationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PrestationMotifNonRealisationCodeType where
    acceptingParser = fmap PrestationMotifNonRealisationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (PrestationMotifNonRealisationCodeType x) = simpleTypeText x
 
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
 
newtype ProductionAutonomeCouplageModeCodeType = ProductionAutonomeCouplageModeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ProductionAutonomeCouplageModeCodeType Xsd.XsdString where
    restricts (ProductionAutonomeCouplageModeCodeType x) = x
instance SchemaType ProductionAutonomeCouplageModeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ProductionAutonomeCouplageModeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ProductionAutonomeCouplageModeCodeType where
    acceptingParser = fmap ProductionAutonomeCouplageModeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ProductionAutonomeCouplageModeCodeType x) = simpleTypeText x
 
newtype ProgrammeCircuitChauffageTempoCodeType = ProgrammeCircuitChauffageTempoCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ProgrammeCircuitChauffageTempoCodeType Xsd.XsdString where
    restricts (ProgrammeCircuitChauffageTempoCodeType x) = x
instance SchemaType ProgrammeCircuitChauffageTempoCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ProgrammeCircuitChauffageTempoCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ProgrammeCircuitChauffageTempoCodeType where
    acceptingParser = fmap ProgrammeCircuitChauffageTempoCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ProgrammeCircuitChauffageTempoCodeType x) = simpleTypeText x
 
newtype ProgrammeCircuitEauTempoCodeType = ProgrammeCircuitEauTempoCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ProgrammeCircuitEauTempoCodeType Xsd.XsdString where
    restricts (ProgrammeCircuitEauTempoCodeType x) = x
instance SchemaType ProgrammeCircuitEauTempoCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ProgrammeCircuitEauTempoCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ProgrammeCircuitEauTempoCodeType where
    acceptingParser = fmap ProgrammeCircuitEauTempoCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [A-Z0-9]{1,15})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (ProgrammeCircuitEauTempoCodeType x) = simpleTypeText x
 
data PuissanceUniteSymboleType
    = PuissanceUniteSymboleType_KVA
    | PuissanceUniteSymboleType_KVAR
    | PuissanceUniteSymboleType_KW
    deriving (Eq,Show,Enum)
instance SchemaType PuissanceUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType PuissanceUniteSymboleType where
    acceptingParser =  do _ <- literal "kVA"; return PuissanceUniteSymboleType_KVA
                      `onFail` do _ <- literal "kVAR"; return PuissanceUniteSymboleType_KVAR
                      `onFail` do _ <- literal "kW"; return PuissanceUniteSymboleType_KW
                      
    simpleTypeText PuissanceUniteSymboleType_KVA = "kVA"
    simpleTypeText PuissanceUniteSymboleType_KVAR = "kVAR"
    simpleTypeText PuissanceUniteSymboleType_KW = "kW"
 
newtype RattachementIdType = RattachementIdType Xsd.XsdString deriving (Eq,Show)
instance Restricts RattachementIdType Xsd.XsdString where
    restricts (RattachementIdType x) = x
instance SchemaType RattachementIdType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RattachementIdType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RattachementIdType where
    acceptingParser = fmap RattachementIdType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RattachementIdType x) = simpleTypeText x
 
newtype RattachementPointRoleCodeType = RattachementPointRoleCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RattachementPointRoleCodeType Xsd.XsdString where
    restricts (RattachementPointRoleCodeType x) = x
instance SchemaType RattachementPointRoleCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RattachementPointRoleCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RattachementPointRoleCodeType where
    acceptingParser = fmap RattachementPointRoleCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RattachementPointRoleCodeType x) = simpleTypeText x
 
newtype RattachementTypeCodeType = RattachementTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RattachementTypeCodeType Xsd.XsdString where
    restricts (RattachementTypeCodeType x) = x
instance SchemaType RattachementTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RattachementTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RattachementTypeCodeType where
    acceptingParser = fmap RattachementTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RattachementTypeCodeType x) = simpleTypeText x
 
newtype RecevabiliteMotifRefusCodeType = RecevabiliteMotifRefusCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RecevabiliteMotifRefusCodeType Xsd.XsdString where
    restricts (RecevabiliteMotifRefusCodeType x) = x
instance SchemaType RecevabiliteMotifRefusCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RecevabiliteMotifRefusCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RecevabiliteMotifRefusCodeType where
    acceptingParser = fmap RecevabiliteMotifRefusCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RecevabiliteMotifRefusCodeType x) = simpleTypeText x
 
newtype RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType = RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType Xsd.XsdString where
    restricts (RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType x) = x
instance SchemaType RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType where
    acceptingParser = fmap RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType x) = simpleTypeText x
 
newtype RecevabiliteResultatCodeType = RecevabiliteResultatCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RecevabiliteResultatCodeType Xsd.XsdString where
    restricts (RecevabiliteResultatCodeType x) = x
instance SchemaType RecevabiliteResultatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RecevabiliteResultatCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RecevabiliteResultatCodeType where
    acceptingParser = fmap RecevabiliteResultatCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RecevabiliteResultatCodeType x) = simpleTypeText x
 
newtype ReclamationSousTypeCodeType = ReclamationSousTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ReclamationSousTypeCodeType Xsd.XsdString where
    restricts (ReclamationSousTypeCodeType x) = x
instance SchemaType ReclamationSousTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ReclamationSousTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReclamationSousTypeCodeType where
    acceptingParser = fmap ReclamationSousTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReclamationSousTypeCodeType x) = simpleTypeText x
 
newtype ReclamationTypeCodeType = ReclamationTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ReclamationTypeCodeType Xsd.XsdString where
    restricts (ReclamationTypeCodeType x) = x
instance SchemaType ReclamationTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ReclamationTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReclamationTypeCodeType where
    acceptingParser = fmap ReclamationTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReclamationTypeCodeType x) = simpleTypeText x
 
newtype RectificationMotifCodeType = RectificationMotifCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RectificationMotifCodeType Xsd.XsdString where
    restricts (RectificationMotifCodeType x) = x
instance SchemaType RectificationMotifCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RectificationMotifCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RectificationMotifCodeType where
    acceptingParser = fmap RectificationMotifCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RectificationMotifCodeType x) = simpleTypeText x
 
newtype RefCompteurType = RefCompteurType Xsd.XsdString deriving (Eq,Show)
instance Restricts RefCompteurType Xsd.XsdString where
    restricts (RefCompteurType x) = x
instance SchemaType RefCompteurType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RefCompteurType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RefCompteurType where
    acceptingParser = fmap RefCompteurType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RefCompteurType x) = simpleTypeText x
 
newtype RelaisCommandeTypeCodeType = RelaisCommandeTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RelaisCommandeTypeCodeType Xsd.XsdString where
    restricts (RelaisCommandeTypeCodeType x) = x
instance SchemaType RelaisCommandeTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RelaisCommandeTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RelaisCommandeTypeCodeType where
    acceptingParser = fmap RelaisCommandeTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RelaisCommandeTypeCodeType x) = simpleTypeText x
 
newtype RelaisNatureCodeType = RelaisNatureCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RelaisNatureCodeType Xsd.XsdString where
    restricts (RelaisNatureCodeType x) = x
instance SchemaType RelaisNatureCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RelaisNatureCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RelaisNatureCodeType where
    acceptingParser = fmap RelaisNatureCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RelaisNatureCodeType x) = simpleTypeText x
 
newtype ReleveMediaCodeType = ReleveMediaCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ReleveMediaCodeType Xsd.XsdString where
    restricts (ReleveMediaCodeType x) = x
instance SchemaType ReleveMediaCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ReleveMediaCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReleveMediaCodeType where
    acceptingParser = fmap ReleveMediaCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReleveMediaCodeType x) = simpleTypeText x
 
newtype ReleveModeCodeType = ReleveModeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ReleveModeCodeType Xsd.XsdString where
    restricts (ReleveModeCodeType x) = x
instance SchemaType ReleveModeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ReleveModeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReleveModeCodeType where
    acceptingParser = fmap ReleveModeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReleveModeCodeType x) = simpleTypeText x
 
newtype RelevePlageCodeType = RelevePlageCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts RelevePlageCodeType Xsd.XsdString where
    restricts (RelevePlageCodeType x) = x
instance SchemaType RelevePlageCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (RelevePlageCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType RelevePlageCodeType where
    acceptingParser = fmap RelevePlageCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (RelevePlageCodeType x) = simpleTypeText x
 
newtype ReleveQualificationCodeType = ReleveQualificationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ReleveQualificationCodeType Xsd.XsdString where
    restricts (ReleveQualificationCodeType x) = x
instance SchemaType ReleveQualificationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ReleveQualificationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReleveQualificationCodeType where
    acceptingParser = fmap ReleveQualificationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReleveQualificationCodeType x) = simpleTypeText x
 
newtype ReleveTraitementModeCodeType = ReleveTraitementModeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ReleveTraitementModeCodeType Xsd.XsdString where
    restricts (ReleveTraitementModeCodeType x) = x
instance SchemaType ReleveTraitementModeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ReleveTraitementModeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ReleveTraitementModeCodeType where
    acceptingParser = fmap ReleveTraitementModeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ReleveTraitementModeCodeType x) = simpleTypeText x
 
data ResidenceTypeCodeType
    = ResidenceTypeCodeType_PRP
    | ResidenceTypeCodeType_SEC
    deriving (Eq,Show,Enum)
instance SchemaType ResidenceTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ResidenceTypeCodeType where
    acceptingParser =  do _ <- literal "PRP"; return ResidenceTypeCodeType_PRP
                      `onFail` do _ <- literal "SEC"; return ResidenceTypeCodeType_SEC
                      
    simpleTypeText ResidenceTypeCodeType_PRP = "PRP"
    simpleTypeText ResidenceTypeCodeType_SEC = "SEC"
 
newtype SecteurGeographiqueCodeType = SecteurGeographiqueCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts SecteurGeographiqueCodeType Xsd.XsdString where
    restricts (SecteurGeographiqueCodeType x) = x
instance SchemaType SecteurGeographiqueCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (SecteurGeographiqueCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType SecteurGeographiqueCodeType where
    acceptingParser = fmap SecteurGeographiqueCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (SecteurGeographiqueCodeType x) = simpleTypeText x
 
data SituationContractuelleGestionModeCodeType
    = SituationContractuelleGestionModeCodeType_INT
    | SituationContractuelleGestionModeCodeType_GRD
    deriving (Eq,Show,Enum)
instance SchemaType SituationContractuelleGestionModeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType SituationContractuelleGestionModeCodeType where
    acceptingParser =  do _ <- literal "INT"; return SituationContractuelleGestionModeCodeType_INT
                      `onFail` do _ <- literal "GRD"; return SituationContractuelleGestionModeCodeType_GRD
                      
    simpleTypeText SituationContractuelleGestionModeCodeType_INT = "INT"
    simpleTypeText SituationContractuelleGestionModeCodeType_GRD = "GRD"
 
newtype StructureComptageCodeType = StructureComptageCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts StructureComptageCodeType Xsd.XsdString where
    restricts (StructureComptageCodeType x) = x
instance SchemaType StructureComptageCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (StructureComptageCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType StructureComptageCodeType where
    acceptingParser = fmap StructureComptageCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (StructureComptageCodeType x) = simpleTypeText x
 
newtype StructureTarifaireContexteUtilisationCodeType = StructureTarifaireContexteUtilisationCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts StructureTarifaireContexteUtilisationCodeType Xsd.XsdString where
    restricts (StructureTarifaireContexteUtilisationCodeType x) = x
instance SchemaType StructureTarifaireContexteUtilisationCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (StructureTarifaireContexteUtilisationCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType StructureTarifaireContexteUtilisationCodeType where
    acceptingParser = fmap StructureTarifaireContexteUtilisationCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (StructureTarifaireContexteUtilisationCodeType x) = simpleTypeText x
 
newtype TensionLivraisonCodeType = TensionLivraisonCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts TensionLivraisonCodeType Xsd.XsdString where
    restricts (TensionLivraisonCodeType x) = x
instance SchemaType TensionLivraisonCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TensionLivraisonCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TensionLivraisonCodeType where
    acceptingParser = fmap TensionLivraisonCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (TensionLivraisonCodeType x) = simpleTypeText x
 
data TensionUniteSymboleType
    = TensionUniteSymboleType_KV
    | TensionUniteSymboleType_V
    deriving (Eq,Show,Enum)
instance SchemaType TensionUniteSymboleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s x = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TensionUniteSymboleType where
    acceptingParser =  do _ <- literal "kV"; return TensionUniteSymboleType_KV
                      `onFail` do _ <- literal "V"; return TensionUniteSymboleType_V
                      
    simpleTypeText TensionUniteSymboleType_KV = "kV"
    simpleTypeText TensionUniteSymboleType_V = "V"
 
newtype TourneeCodeType = TourneeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts TourneeCodeType Xsd.XsdString where
    restricts (TourneeCodeType x) = x
instance SchemaType TourneeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TourneeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TourneeCodeType where
    acceptingParser = fmap TourneeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Pattern [0-9A-Z]{2})
    --      (Enumeration)
    --      (StrLength (Occurs Nothing Nothing))
    simpleTypeText (TourneeCodeType x) = simpleTypeText x
 
newtype TransformateurCalibreCodeType = TransformateurCalibreCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts TransformateurCalibreCodeType Xsd.XsdString where
    restricts (TransformateurCalibreCodeType x) = x
instance SchemaType TransformateurCalibreCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TransformateurCalibreCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TransformateurCalibreCodeType where
    acceptingParser = fmap TransformateurCalibreCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (TransformateurCalibreCodeType x) = simpleTypeText x
 
newtype TransformateurCouplageTypeCodeType = TransformateurCouplageTypeCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts TransformateurCouplageTypeCodeType Xsd.XsdString where
    restricts (TransformateurCouplageTypeCodeType x) = x
instance SchemaType TransformateurCouplageTypeCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TransformateurCouplageTypeCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TransformateurCouplageTypeCodeType where
    acceptingParser = fmap TransformateurCouplageTypeCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (TransformateurCouplageTypeCodeType x) = simpleTypeText x
 
newtype TransformateurCourantPositionCodeType = TransformateurCourantPositionCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts TransformateurCourantPositionCodeType Xsd.XsdString where
    restricts (TransformateurCourantPositionCodeType x) = x
instance SchemaType TransformateurCourantPositionCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TransformateurCourantPositionCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TransformateurCourantPositionCodeType where
    acceptingParser = fmap TransformateurCourantPositionCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (TransformateurCourantPositionCodeType x) = simpleTypeText x
 
newtype TransformateurPrecisionClasseCodeType = TransformateurPrecisionClasseCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts TransformateurPrecisionClasseCodeType Xsd.XsdString where
    restricts (TransformateurPrecisionClasseCodeType x) = x
instance SchemaType TransformateurPrecisionClasseCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (TransformateurPrecisionClasseCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType TransformateurPrecisionClasseCodeType where
    acceptingParser = fmap TransformateurPrecisionClasseCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (TransformateurPrecisionClasseCodeType x) = simpleTypeText x
 
newtype UsageChantierCodeType = UsageChantierCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts UsageChantierCodeType Xsd.XsdString where
    restricts (UsageChantierCodeType x) = x
instance SchemaType UsageChantierCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (UsageChantierCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType UsageChantierCodeType where
    acceptingParser = fmap UsageChantierCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (UsageChantierCodeType x) = simpleTypeText x
 
type UtilisateurLoginType = AdresseEmailType -- modification
-- Placeholder for a Union type, not yet implemented.
 
newtype ZoneQualiteDesserteCodeType = ZoneQualiteDesserteCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ZoneQualiteDesserteCodeType Xsd.XsdString where
    restricts (ZoneQualiteDesserteCodeType x) = x
instance SchemaType ZoneQualiteDesserteCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ZoneQualiteDesserteCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ZoneQualiteDesserteCodeType where
    acceptingParser = fmap ZoneQualiteDesserteCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    simpleTypeText (ZoneQualiteDesserteCodeType x) = simpleTypeText x
