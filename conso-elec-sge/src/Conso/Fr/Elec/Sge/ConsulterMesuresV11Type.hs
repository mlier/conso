{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.ConsulterMesuresV11Type
  ( module Conso.Fr.Elec.Sge.ConsulterMesuresV11Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( OneOf2(..),
      Content,
      XMLParser,
      SchemaType(..),
      foldOneOf2,
      optional,
      apply,
      Commitment(oneOf', commit),
      between,
      toXMLAttribute,
      toXMLElement,
      interior,
      posnElement,
      Occurs(Occurs) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc
    ( CalendrierType,
      ClasseTemporelleType,
      GrandeurPhysiqueType,
      MesureDeclencheurType,
      MesureNatureType,
      MesureStatutType )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType,
      UtilisateurLoginType,
      ContratIdType,
      BooleenType,
      NbEntierType,
      IndexUniteSymboleType,
      DateType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementConsulterMesures :: XMLParser ConsulterMesuresType
elementConsulterMesures = parseSchemaType "sc:consulterMesures"
elementToXMLConsulterMesures :: ConsulterMesuresType -> [Content ()]
elementToXMLConsulterMesures = schemaTypeToXML "sc:consulterMesures"
 
data ConsulterMesuresType = ConsulterMesuresType
        { consulterMesuresType_pointId :: Ds.PointIdType
        , consulterMesuresType_loginDemandeur :: Ds.UtilisateurLoginType
        , consulterMesuresType_contratId :: Ds.ContratIdType
        , consulterMesuresType_choice3 :: Maybe (OneOf2 Ds.BooleenType Ds.BooleenType)
          -- ^ Choice between:
          --   
          --   (1) contratConcluNouveauClientSurSite
          --   
          --   (2) autorisationClient
        }
        deriving (Eq,Show)
instance SchemaType ConsulterMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ConsulterMesuresType
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "loginDemandeur"
            `apply` parseSchemaType "contratId"
            `apply` optional (oneOf' [ ("Ds.BooleenType", fmap OneOf2 (parseSchemaType "contratConcluNouveauClientSurSite"))
                                     , ("Ds.BooleenType", fmap TwoOf2 (parseSchemaType "autorisationClient"))
                                     ])
    schemaTypeToXML s x@ConsulterMesuresType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/services/consultermesures/v1.1" 
                       ]
            [ schemaTypeToXML "pointId" $ consulterMesuresType_pointId x
            , schemaTypeToXML "loginDemandeur" $ consulterMesuresType_loginDemandeur x
            , schemaTypeToXML "contratId" $ consulterMesuresType_contratId x
            , maybe [] (foldOneOf2  (schemaTypeToXML "contratConcluNouveauClientSurSite")
                                    (schemaTypeToXML "autorisationClient")
                                   ) $ consulterMesuresType_choice3 x
            ]
 
elementConsulterMesuresResponse :: XMLParser ConsulterMesuresResponseType
elementConsulterMesuresResponse = parseSchemaType "ns4:consulterMesuresResponse"
elementToXMLConsulterMesuresResponse :: ConsulterMesuresResponseType -> [Content ()]
elementToXMLConsulterMesuresResponse = schemaTypeToXML "ns4:consulterMesuresResponse"
 
data ConsulterMesuresResponseType = ConsulterMesuresResponseType
        { consulterMesuresResponseType_seriesMesuresDateesGrilleTurpe :: Maybe SeriesMesuresDateesType
        , consulterMesuresResponseType_seriesMesuresDateesGrilleFrn :: Maybe SeriesMesuresDateesType
        }
        deriving (Eq,Show)
instance SchemaType ConsulterMesuresResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ConsulterMesuresResponseType
            `apply` optional (parseSchemaType "seriesMesuresDateesGrilleTurpe")
            `apply` optional (parseSchemaType "seriesMesuresDateesGrilleFrn")
    schemaTypeToXML s x@ConsulterMesuresResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "seriesMesuresDateesGrilleTurpe") $ consulterMesuresResponseType_seriesMesuresDateesGrilleTurpe x
            , maybe [] (schemaTypeToXML "seriesMesuresDateesGrilleFrn") $ consulterMesuresResponseType_seriesMesuresDateesGrilleFrn x
            ]
 
data MesureDateeType = MesureDateeType
        { mesureDateeType_valeur :: Ds.NbEntierType
        , mesureDateeType_dateDebut :: Ds.DateType
        , mesureDateeType_dateFin :: Ds.DateType
        , mesureDateeType_nature :: Dc.MesureNatureType
        , mesureDateeType_declencheur :: Maybe Dc.MesureDeclencheurType
        , mesureDateeType_statut :: Maybe Dc.MesureStatutType
        }
        deriving (Eq,Show)
instance SchemaType MesureDateeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return MesureDateeType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "dateDebut"
            `apply` parseSchemaType "dateFin"
            `apply` parseSchemaType "nature"
            `apply` optional (parseSchemaType "declencheur")
            `apply` optional (parseSchemaType "statut")
    schemaTypeToXML s x@MesureDateeType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ mesureDateeType_valeur x
            , schemaTypeToXML "dateDebut" $ mesureDateeType_dateDebut x
            , schemaTypeToXML "dateFin" $ mesureDateeType_dateFin x
            , schemaTypeToXML "nature" $ mesureDateeType_nature x
            , maybe [] (schemaTypeToXML "declencheur") $ mesureDateeType_declencheur x
            , maybe [] (schemaTypeToXML "statut") $ mesureDateeType_statut x
            ]
 
newtype SeriesMesuresDateesType = SeriesMesuresDateesType
        { seriesMesuresDateesType_serie :: [SerieMesuresDateesType]
        }
        deriving (Eq,Show)
instance SchemaType SeriesMesuresDateesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return SeriesMesuresDateesType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "serie")
    schemaTypeToXML s x@SeriesMesuresDateesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serie") $ seriesMesuresDateesType_serie x
            ]
 
data SerieMesuresDateesType = SerieMesuresDateesType
        { serieMesuresDateesType_grandeurPhysique :: Dc.GrandeurPhysiqueType
        , serieMesuresDateesType_classeTemporelle :: Maybe Dc.ClasseTemporelleType
        , serieMesuresDateesType_calendrier :: Maybe Dc.CalendrierType
        , serieMesuresDateesType_unite :: Ds.IndexUniteSymboleType
        , serieMesuresDateesType_mesuresDatees :: Maybe MesuresDateesType
        }
        deriving (Eq,Show)
instance SchemaType SerieMesuresDateesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return SerieMesuresDateesType
            `apply` parseSchemaType "grandeurPhysique"
            `apply` optional (parseSchemaType "classeTemporelle")
            `apply` optional (parseSchemaType "calendrier")
            `apply` parseSchemaType "unite"
            `apply` optional (parseSchemaType "mesuresDatees")
    schemaTypeToXML s x@SerieMesuresDateesType{} =
        toXMLElement s []
            [ schemaTypeToXML "grandeurPhysique" $ serieMesuresDateesType_grandeurPhysique x
            , maybe [] (schemaTypeToXML "classeTemporelle") $ serieMesuresDateesType_classeTemporelle x
            , maybe [] (schemaTypeToXML "calendrier") $ serieMesuresDateesType_calendrier x
            , schemaTypeToXML "unite" $ serieMesuresDateesType_unite x
            , maybe [] (schemaTypeToXML "mesuresDatees") $ serieMesuresDateesType_mesuresDatees x
            ]
 
newtype MesuresDateesType = MesuresDateesType
        { mesuresDateesType_mesure :: [MesureDateeType]
        }
        deriving (Eq,Show)
instance SchemaType MesuresDateesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return MesuresDateesType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "mesure")
    schemaTypeToXML s x@MesuresDateesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "mesure") $ mesuresDateesType_mesure x
            ]
