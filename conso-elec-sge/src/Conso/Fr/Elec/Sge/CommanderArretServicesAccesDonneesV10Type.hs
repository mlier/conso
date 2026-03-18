{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
  ( module Conso.Fr.Elec.Sge.CommanderArretServicesAccesDonneesV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderArretServicesAccesDonnees :: XMLParser CommanderArretServicesAccesDonneesType
elementCommanderArretServicesAccesDonnees = parseSchemaType "commanderArretServicesAccesDonnees"
elementToXMLCommanderArretServicesAccesDonnees :: CommanderArretServicesAccesDonneesType -> [Content ()]
elementToXMLCommanderArretServicesAccesDonnees = schemaTypeToXML "sc:commanderArretServicesAccesDonnees"
 
elementCommanderArretServicesAccesDonneesResponse :: XMLParser CommanderArretServicesAccesDonneesResponseType
elementCommanderArretServicesAccesDonneesResponse = parseSchemaType "commanderArretServicesAccesDonneesResponse"
elementToXMLCommanderArretServicesAccesDonneesResponse :: CommanderArretServicesAccesDonneesResponseType -> [Content ()]
elementToXMLCommanderArretServicesAccesDonneesResponse = schemaTypeToXML "commanderArretServicesAccesDonneesResponse"
 
newtype CommanderArretServicesAccesDonneesType = CommanderArretServicesAccesDonneesType
        { commanderArretServicesAccesDonneesType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderArretServicesAccesDonneesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderArretServicesAccesDonneesType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderArretServicesAccesDonneesType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/ws/commanderarretservicesaccesdonnees/v1.0"
                       ]
            [ schemaTypeToXML "demande" $ commanderArretServicesAccesDonneesType_demande x
            ]
 
newtype CommanderArretServicesAccesDonneesResponseType = CommanderArretServicesAccesDonneesResponseType
        { commanderArretServicesAccesDonneesResponseType_servicesResilies :: ServicesResiliesType
        }
        deriving (Eq,Show)
instance SchemaType CommanderArretServicesAccesDonneesResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderArretServicesAccesDonneesResponseType
            `apply` parseSchemaType "servicesResilies"
    schemaTypeToXML s x@CommanderArretServicesAccesDonneesResponseType{} =
        toXMLElement s []
            [ schemaTypeToXML "servicesResilies" $ commanderArretServicesAccesDonneesResponseType_servicesResilies x
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
        { donneesGeneralesType_refExterne :: Maybe Ds.Chaine255Type
        , donneesGeneralesType_pointId :: Ds.PointIdType
        , donneesGeneralesType_initiateurLogin :: Ds.UtilisateurLoginType
        , donneesGeneralesType_contratId :: Ds.ContratIdType
        , donneesGeneralesType_sens :: SensType
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
            `apply` parseSchemaType "sens"
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "refExterne") $ donneesGeneralesType_refExterne x
            , schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGeneralesType_contratId x
            , schemaTypeToXML "sens" $ donneesGeneralesType_sens x
            ]
 
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
 
newtype ServicesSouscritsType = ServicesSouscritsType
        { servicesSouscritsType_serviceSouscritId :: [Ds.Chaine15Type]
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
 
newtype ServicesResiliesType = ServicesResiliesType
        { servicesResiliesType_serviceResilie :: [ServiceResilieType]
        }
        deriving (Eq,Show)
instance SchemaType ServicesResiliesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServicesResiliesType
            `apply` many1 (parseSchemaType "serviceResilie")
    schemaTypeToXML s x@ServicesResiliesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "serviceResilie") $ servicesResiliesType_serviceResilie x
            ]
 
data ServiceResilieType = ServiceResilieType
        { serviceResilieType_affaireId :: Ds.AffaireIdType
        , serviceResilieType_serviceSouscritId :: Ds.Chaine15Type
        }
        deriving (Eq,Show)
instance SchemaType ServiceResilieType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ServiceResilieType
            `apply` parseSchemaType "affaireId"
            `apply` parseSchemaType "serviceSouscritId"
    schemaTypeToXML s x@ServiceResilieType{} =
        toXMLElement s []
            [ schemaTypeToXML "affaireId" $ serviceResilieType_affaireId x
            , schemaTypeToXML "serviceSouscritId" $ serviceResilieType_serviceSouscritId x
            ]
