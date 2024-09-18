{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type
  ( module Conso.Fr.Elec.Sge.RechercherServicesSouscritsMesuresV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( SchemaType(..),
      XMLParser,
      Content,
      optional,
      apply,
      Commitment(commit),
      between,
      getAttribute,
      toXMLAttribute,
      toXMLElement,
      interior,
      posnElement,
      Occurs(Occurs) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( BooleenType,
      Chaine15Type,
      Chaine255Type,
      ContratIdType,
      DateType,
      MesureTypeCodeType,
      PointIdType,
      UtilisateurLoginType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data CriteresType = CriteresType
        { criteresType_pointId :: Ds.PointIdType
        , criteresType_contratId :: Ds.ContratIdType
        }
        deriving (Eq,Show)
instance SchemaType CriteresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CriteresType
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "contratId"
    schemaTypeToXML s x@CriteresType{} =
        toXMLElement s []
            [ schemaTypeToXML "pointId" $ criteresType_pointId x
            , schemaTypeToXML "contratId" $ criteresType_contratId x
            ]
 
elementRechercherServicesSouscritsMesures :: XMLParser RechercherServicesSouscritsMesuresType
elementRechercherServicesSouscritsMesures = parseSchemaType "sc:rechercherServicesSouscritsMesures"
elementToXMLRechercherServicesSouscritsMesures :: RechercherServicesSouscritsMesuresType -> [Content ()]
elementToXMLRechercherServicesSouscritsMesures = schemaTypeToXML "sc:rechercherServicesSouscritsMesures"
 
elementRechercherServicesSouscritsMesuresResponse :: XMLParser RechercherServicesSouscritsMesuresResponseType
elementRechercherServicesSouscritsMesuresResponse = parseSchemaType "ns4:rechercherServicesSouscritsMesuresResponse"
elementToXMLRechercherServicesSouscritsMesuresResponse :: RechercherServicesSouscritsMesuresResponseType -> [Content ()]
elementToXMLRechercherServicesSouscritsMesuresResponse = schemaTypeToXML "ns4:rechercherServicesSouscritsMesuresResponse"
 
newtype RechercherServicesSouscritsMesuresResponseType = RechercherServicesSouscritsMesuresResponseType
        { rechercherServicesSouscritsMesuresResponseType_servicesSouscritsMesures :: Maybe ServicesSouscritsMesuresType
        }
        deriving (Eq,Show)
instance SchemaType RechercherServicesSouscritsMesuresResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RechercherServicesSouscritsMesuresResponseType
            `apply` optional (parseSchemaType "servicesSouscritsMesures")
    schemaTypeToXML s x@RechercherServicesSouscritsMesuresResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "servicesSouscritsMesures") $ rechercherServicesSouscritsMesuresResponseType_servicesSouscritsMesures x
            ]
 
data RechercherServicesSouscritsMesuresType = RechercherServicesSouscritsMesuresType
        { rechercherServicesSouscritsMesuresType_criteres :: CriteresType
        , rechercherServicesSouscritsMesuresType_loginUtilisateur :: Ds.UtilisateurLoginType
        }
        deriving (Eq,Show)
instance SchemaType RechercherServicesSouscritsMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RechercherServicesSouscritsMesuresType
            `apply` parseSchemaType "criteres"
            `apply` parseSchemaType "loginUtilisateur"
    schemaTypeToXML s x@RechercherServicesSouscritsMesuresType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/rechercherservicessouscritsmesures/v1.0"  
        ]
            [ schemaTypeToXML "criteres" $ rechercherServicesSouscritsMesuresType_criteres x
            , schemaTypeToXML "loginUtilisateur" $ rechercherServicesSouscritsMesuresType_loginUtilisateur x
            ]
 
data ServiceSouscritMesuresType = ServiceSouscritMesuresType
        { serviceSouscritMesuresType_serviceSouscritId :: Ds.Chaine15Type
        , serviceSouscritMesuresType_pointId :: Ds.PointIdType
        , serviceSouscritMesuresType_serviceSouscritType :: ServiceSouscritType
        , serviceSouscritMesuresType_serviceSouscritLibelle :: Ds.Chaine255Type
        , serviceSouscritMesuresType_injection :: Maybe Ds.BooleenType
        , serviceSouscritMesuresType_soutirage :: Maybe Ds.BooleenType
        , serviceSouscritMesuresType_contratId :: Maybe Ds.ContratIdType
        , serviceSouscritMesuresType_contratLibelle :: Maybe Ds.Chaine255Type
        , serviceSouscritMesuresType_etatCode :: Ds.Chaine15Type
        , serviceSouscritMesuresType_dateDebut :: Ds.DateType
        , serviceSouscritMesuresType_dateFin :: Maybe Ds.DateType
        , serviceSouscritMesuresType_motifFinLibelle :: Maybe Ds.Chaine255Type
        , serviceSouscritMesuresType_mesuresTypeCode :: Maybe Ds.MesureTypeCodeType
        , serviceSouscritMesuresType_mesuresPas :: Maybe Ds.Chaine15Type
        , serviceSouscritMesuresType_mesuresCorrigees :: Maybe Ds.BooleenType
        , serviceSouscritMesuresType_periodiciteTransmission :: Maybe Ds.Chaine15Type
        }
        deriving (Eq,Show)
instance SchemaType ServiceSouscritMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceSouscritMesuresType
            `apply` parseSchemaType "serviceSouscritId"
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "serviceSouscritType"
            `apply` parseSchemaType "serviceSouscritLibelle"
            `apply` optional (parseSchemaType "injection")
            `apply` optional (parseSchemaType "soutirage")
            `apply` optional (parseSchemaType "contratId")
            `apply` optional (parseSchemaType "contratLibelle")
            `apply` parseSchemaType "etatCode"
            `apply` parseSchemaType "dateDebut"
            `apply` optional (parseSchemaType "dateFin")
            `apply` optional (parseSchemaType "motifFinLibelle")
            `apply` optional (parseSchemaType "mesuresTypeCode")
            `apply` optional (parseSchemaType "mesuresPas")
            `apply` optional (parseSchemaType "mesuresCorrigees")
            `apply` optional (parseSchemaType "periodiciteTransmission")
    schemaTypeToXML s x@ServiceSouscritMesuresType{} =
        toXMLElement s []
            [ schemaTypeToXML "serviceSouscritId" $ serviceSouscritMesuresType_serviceSouscritId x
            , schemaTypeToXML "pointId" $ serviceSouscritMesuresType_pointId x
            , schemaTypeToXML "serviceSouscritType" $ serviceSouscritMesuresType_serviceSouscritType x
            , schemaTypeToXML "serviceSouscritLibelle" $ serviceSouscritMesuresType_serviceSouscritLibelle x
            , maybe [] (schemaTypeToXML "injection") $ serviceSouscritMesuresType_injection x
            , maybe [] (schemaTypeToXML "soutirage") $ serviceSouscritMesuresType_soutirage x
            , maybe [] (schemaTypeToXML "contratId") $ serviceSouscritMesuresType_contratId x
            , maybe [] (schemaTypeToXML "contratLibelle") $ serviceSouscritMesuresType_contratLibelle x
            , schemaTypeToXML "etatCode" $ serviceSouscritMesuresType_etatCode x
            , schemaTypeToXML "dateDebut" $ serviceSouscritMesuresType_dateDebut x
            , maybe [] (schemaTypeToXML "dateFin") $ serviceSouscritMesuresType_dateFin x
            , maybe [] (schemaTypeToXML "motifFinLibelle") $ serviceSouscritMesuresType_motifFinLibelle x
            , maybe [] (schemaTypeToXML "mesuresTypeCode") $ serviceSouscritMesuresType_mesuresTypeCode x
            , maybe [] (schemaTypeToXML "mesuresPas") $ serviceSouscritMesuresType_mesuresPas x
            , maybe [] (schemaTypeToXML "mesuresCorrigees") $ serviceSouscritMesuresType_mesuresCorrigees x
            , maybe [] (schemaTypeToXML "periodiciteTransmission") $ serviceSouscritMesuresType_periodiciteTransmission x
            ]
 
data ServiceSouscritType = ServiceSouscritType
        { serviceSouscritType_code :: Ds.Chaine15Type
        , serviceSouscritType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ServiceSouscritType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ServiceSouscritType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ServiceSouscritType{} =
        toXMLElement s [ toXMLAttribute "code" $ serviceSouscritType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ serviceSouscritType_libelle x
            ]
 
newtype ServicesSouscritsMesuresType = ServicesSouscritsMesuresType
        { servicesSouscritsMesuresType_serviceSouscritMesures :: [ServiceSouscritMesuresType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesSouscritsMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesSouscritsMesuresType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "serviceSouscritMesures")
    schemaTypeToXML s x@ServicesSouscritsMesuresType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceSouscritMesures") $ servicesSouscritsMesuresType_serviceSouscritMesures x
            ]
