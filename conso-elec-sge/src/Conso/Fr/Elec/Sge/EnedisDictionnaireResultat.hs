{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Conso.Fr.Elec.Sge.EnedisDictionnaireResultat
  ( module Conso.Fr.Elec.Sge.EnedisDictionnaireResultat
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( XMLParser,
      SchemaType(..),
      Content(CElem),
      Commitment(commit),
      SimpleType(..),
      Restricts(..),
      posnElement,
      interior,
      toXMLElement,
      getAttribute,
      toXMLAttribute,
      element,
      parseSimpleType,
      toXMLText,
      addXMLAttributes,
      reparse,
      Extension(..) )
import Text.XML.HaXml.OneOfN ()
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd


elementResultat :: XMLParser ResultatType
elementResultat = parseSchemaType "resultat"
elementToXMLResultat :: ResultatType -> [Content ()]
elementToXMLResultat = schemaTypeToXML "resultat"

data ResultatType = ResultatType ResultatLibelleType ResultatTypeAttributes deriving (Eq,Show)

newtype ResultatTypeAttributes = ResultatTypeAttributes
    { resultatTypeAttributes_code :: ResultatCodeType
    }
    deriving (Eq,Show)
instance SchemaType ResultatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "code" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ ResultatType v (ResultatTypeAttributes a0)
    schemaTypeToXML s (ResultatType bt at) =
        addXMLAttributes [ toXMLAttribute "code" $ resultatTypeAttributes_code at
                         ]
            $ schemaTypeToXML s bt
instance Extension ResultatType ResultatLibelleType where
    supertype (ResultatType s _) = s


newtype ResultatLibelleType = ResultatLibelleType Xsd.XsdString deriving (Eq,Show)
instance Restricts ResultatLibelleType Xsd.XsdString where
    restricts (ResultatLibelleType x) = x
instance SchemaType ResultatLibelleType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ResultatLibelleType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ResultatLibelleType where
    acceptingParser = fmap ResultatLibelleType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ResultatLibelleType x) = simpleTypeText x


newtype ResultatCodeType = ResultatCodeType Xsd.XsdString deriving (Eq,Show)
instance Restricts ResultatCodeType Xsd.XsdString where
    restricts (ResultatCodeType x) = x
instance SchemaType ResultatCodeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e parseSimpleType
    schemaTypeToXML s (ResultatCodeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType ResultatCodeType where
    acceptingParser = fmap ResultatCodeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs Nothing Nothing))
    --      (Enumeration)
    simpleTypeText (ResultatCodeType x) = simpleTypeText x