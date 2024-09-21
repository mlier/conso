{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type
  ( module Conso.Fr.Elec.Sge.CommanderArretServiceSouscritMesuresV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( SchemaType(..),
      XMLParser,
      Content,
      between,
      toXMLAttribute,
      toXMLElement,
      interior,
      posnElement,
      optional,
      apply,
      Occurs(Occurs),
      Commitment(commit) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd

import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc
    ( PrestationOptionType, PrestationFicheType, PrestationCasType )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( UtilisateurLoginType,
      PointIdType,
      NbEntierType,
      DemandeObjetCodeType,
      ContratIdType,
      Chaine255Type,
      Chaine15Type(..),
      AffaireIdType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderArretServiceSouscritMesures :: XMLParser CommanderArretServiceSouscritMesuresType
elementCommanderArretServiceSouscritMesures = parseSchemaType "sc:commanderArretServiceSouscritMesures"
elementToXMLCommanderArretServiceSouscritMesures :: CommanderArretServiceSouscritMesuresType -> [Content ()]
elementToXMLCommanderArretServiceSouscritMesures = schemaTypeToXML "sc:commanderArretServiceSouscritMesures"
 
elementCommanderArretServiceSouscritMesuresResponse :: XMLParser CommanderArretServiceSouscritMesuresResponseType
elementCommanderArretServiceSouscritMesuresResponse = parseSchemaType "ns4:commanderArretServiceSouscritMesuresResponse"
elementToXMLCommanderArretServiceSouscritMesuresResponse :: CommanderArretServiceSouscritMesuresResponseType -> [Content ()]
elementToXMLCommanderArretServiceSouscritMesuresResponse = schemaTypeToXML "ns4:commanderArretServiceSouscritMesuresResponse"
 
newtype CommanderArretServiceSouscritMesuresType = CommanderArretServiceSouscritMesuresType
        { commanderArretServiceSouscritMesuresType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderArretServiceSouscritMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderArretServiceSouscritMesuresType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderArretServiceSouscritMesuresType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/commanderarretservicesouscritmesures/v1.0" 
                       ]
            [ schemaTypeToXML "demande" $ commanderArretServiceSouscritMesuresType_demande x
            ]
 
data CommanderArretServiceSouscritMesuresResponseType = CommanderArretServiceSouscritMesuresResponseType
        { commanderArretServiceSouscritMesuresResponseType_affaireId :: Maybe Ds.AffaireIdType
        , commanderArretServiceSouscritMesuresResponseType_prestations :: Maybe PrestationsType
        }
        deriving (Eq,Show)
instance SchemaType CommanderArretServiceSouscritMesuresResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderArretServiceSouscritMesuresResponseType
            `apply` optional (parseSchemaType "affaireId")
            `apply` optional (parseSchemaType "prestations")
    schemaTypeToXML s x@CommanderArretServiceSouscritMesuresResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "affaireId") $ commanderArretServiceSouscritMesuresResponseType_affaireId x
            , maybe [] (schemaTypeToXML "prestations") $ commanderArretServiceSouscritMesuresResponseType_prestations x
            ]
 
data DemandeType = DemandeType
        { demandeType_donneesGenerales :: DonneesGeneralesType
        , demandeType_arretServiceSouscrit :: ArretServiceSouscritType
        }
        deriving (Eq,Show)
instance SchemaType DemandeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandeType
            `apply` parseSchemaType "donneesGenerales"
            `apply` parseSchemaType "arretServiceSouscrit"
    schemaTypeToXML s x@DemandeType{} =
        toXMLElement s []
            [ schemaTypeToXML "donneesGenerales" $ demandeType_donneesGenerales x
            , schemaTypeToXML "arretServiceSouscrit" $ demandeType_arretServiceSouscrit x
            ]
 
data DonneesGeneralesType = DonneesGeneralesType
        { donneesGeneralesType_refFrn :: Maybe Ds.Chaine255Type
        , donneesGeneralesType_objetCode :: Ds.DemandeObjetCodeType
        , donneesGeneralesType_pointId :: Ds.PointIdType
        , donneesGeneralesType_initiateurLogin :: Ds.UtilisateurLoginType
        , donneesGeneralesType_contratId :: Ds.ContratIdType
        }
        deriving (Eq,Show)
instance SchemaType DonneesGeneralesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DonneesGeneralesType
            `apply` optional (parseSchemaType "refFrn")
            `apply` parseSchemaType "objetCode"
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "contratId"
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "refFrn") $ donneesGeneralesType_refFrn x
            , schemaTypeToXML "objetCode" $ donneesGeneralesType_objetCode x
            , schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGeneralesType_contratId x
            ]
 
newtype ArretServiceSouscritType = ArretServiceSouscritType
        { arretServiceSouscritType_serviceSouscritId :: Ds.Chaine15Type
        }
        deriving (Eq,Show)
instance SchemaType ArretServiceSouscritType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ArretServiceSouscritType
            `apply` parseSchemaType "serviceSouscritId"
    schemaTypeToXML s x@ArretServiceSouscritType{} =
        toXMLElement s []
            [ schemaTypeToXML "serviceSouscritId" $ arretServiceSouscritType_serviceSouscritId x
            ]
 
newtype PrestationsType = PrestationsType
        { prestationsType_prestation :: [PrestationType]
        }
        deriving (Eq,Show)
instance SchemaType PrestationsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PrestationsType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "prestation")
    schemaTypeToXML s x@PrestationsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "prestation") $ prestationsType_prestation x
            ]
 
data PrestationType = PrestationType
        { prestationType_rang :: Ds.NbEntierType
        , prestationType_fiche :: Dc.PrestationFicheType
        , prestationType_option :: Maybe Dc.PrestationOptionType
        , prestationType_cas :: Maybe Dc.PrestationCasType
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
