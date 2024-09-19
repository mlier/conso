{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type
  ( module Conso.Fr.Elec.Sge.CommanderCollectePublicationMesuresV30Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( Content,
      XMLParser,
      foldOneOf2,
      between,
      toXMLAttribute,
      toXMLElement,
      interior,
      posnElement,
      optional,
      apply,
      OneOf2(..),
      SchemaType(..),
      Occurs(Occurs),
      Commitment(commit, oneOf') )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc
    ( PrestationOptionType, PrestationFicheType )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( PointIdType,
      UtilisateurLoginType,
      BooleenType,
      PeriodiciteCodeType,
      OptionContractuelleSouscriteIdType,
      NbEntierType,
      MesureTypeCodeType,
      DemandeObjetCodeType,
      DateType,
      ContratIdType,
      CiviliteAbreviationType,
      Chaine255Type(..),
      Chaine15Type,
      AffaireIdType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderCollectePublicationMesures :: XMLParser CommanderCollectePublicationMesuresType
elementCommanderCollectePublicationMesures = parseSchemaType "sc:commanderCollectePublicationMesures"
elementToXMLCommanderCollectePublicationMesures :: CommanderCollectePublicationMesuresType -> [Content ()]
elementToXMLCommanderCollectePublicationMesures = schemaTypeToXML "sc:commanderCollectePublicationMesures"
 
elementCommanderCollectePublicationMesuresResponse :: XMLParser CommanderCollectePublicationMesuresResponseType
elementCommanderCollectePublicationMesuresResponse = parseSchemaType "ns4:commanderCollectePublicationMesuresResponse"
elementToXMLCommanderCollectePublicationMesuresResponse :: CommanderCollectePublicationMesuresResponseType -> [Content ()]
elementToXMLCommanderCollectePublicationMesuresResponse = schemaTypeToXML "ns4:commanderCollectePublicationMesuresResponse"
 
data CommanderCollectePublicationMesuresResponseType = CommanderCollectePublicationMesuresResponseType
        { commanderCollectePublicationMesuresResponseType_affaireId :: Maybe Ds.AffaireIdType
        , commanderCollectePublicationMesuresResponseType_prestations :: Maybe PrestationsType
        , commanderCollectePublicationMesuresResponseType_serviceSouscritId :: Maybe Ds.OptionContractuelleSouscriteIdType
        }
        deriving (Eq,Show)
instance SchemaType CommanderCollectePublicationMesuresResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderCollectePublicationMesuresResponseType
            `apply` optional (parseSchemaType "affaireId")
            `apply` optional (parseSchemaType "prestations")
            `apply` optional (parseSchemaType "serviceSouscritId")
    schemaTypeToXML s x@CommanderCollectePublicationMesuresResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "affaireId") $ commanderCollectePublicationMesuresResponseType_affaireId x
            , maybe [] (schemaTypeToXML "prestations") $ commanderCollectePublicationMesuresResponseType_prestations x
            , maybe [] (schemaTypeToXML "serviceSouscritId") $ commanderCollectePublicationMesuresResponseType_serviceSouscritId x
            ]
 
newtype CommanderCollectePublicationMesuresType = CommanderCollectePublicationMesuresType
        { commanderCollectePublicationMesuresType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderCollectePublicationMesuresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderCollectePublicationMesuresType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderCollectePublicationMesuresType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/commandercollectepublicationmesures/v3.0" 
                       ]
            [ schemaTypeToXML "demande" $ commanderCollectePublicationMesuresType_demande x
            ]
 
data DeclarationAccordClientType = DeclarationAccordClientType
        { declarationAccordClientType_accord :: Ds.BooleenType
        , declarationAccordClientType_choice1 :: OneOf2 PersonnePhysiqueType PersonneMoraleType
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
            `apply` oneOf' [ ("PersonnePhysiqueType", fmap OneOf2 (parseSchemaType "personnePhysique"))
                           , ("PersonneMoraleType", fmap TwoOf2 (parseSchemaType "personneMorale"))
                           ]
    schemaTypeToXML s x@DeclarationAccordClientType{} =
        toXMLElement s []
            [ schemaTypeToXML "accord" $ declarationAccordClientType_accord x
            , foldOneOf2  (schemaTypeToXML "personnePhysique")
                          (schemaTypeToXML "personneMorale")
                          $ declarationAccordClientType_choice1 x
            ]
 
data DemandeAccesMesures = DemandeAccesMesures
        { demandeAccesMesures_dateDebut :: Ds.DateType
        , demandeAccesMesures_dateFin :: Maybe Ds.DateType
        , demandeAccesMesures_declarationAccordClient :: DeclarationAccordClientType
        , demandeAccesMesures_mesuresTypeCode :: Ds.MesureTypeCodeType
        , demandeAccesMesures_soutirage :: Ds.BooleenType
        , demandeAccesMesures_injection :: Ds.BooleenType
        , demandeAccesMesures_mesuresPas :: Maybe Ds.Chaine15Type
        , demandeAccesMesures_mesuresCorrigees :: Maybe Ds.BooleenType
        , demandeAccesMesures_transmissionRecurrente :: Ds.BooleenType
        , demandeAccesMesures_periodiciteTransmission :: Maybe Ds.PeriodiciteCodeType
        }
        deriving (Eq,Show)
instance SchemaType DemandeAccesMesures where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandeAccesMesures
            `apply` parseSchemaType "dateDebut"
            `apply` optional (parseSchemaType "dateFin")
            `apply` parseSchemaType "declarationAccordClient"
            `apply` parseSchemaType "mesuresTypeCode"
            `apply` parseSchemaType "soutirage"
            `apply` parseSchemaType "injection"
            `apply` optional (parseSchemaType "mesuresPas")
            `apply` optional (parseSchemaType "mesuresCorrigees")
            `apply` parseSchemaType "transmissionRecurrente"
            `apply` optional (parseSchemaType "periodiciteTransmission")
    schemaTypeToXML s x@DemandeAccesMesures{} =
        toXMLElement s []
            [ schemaTypeToXML "dateDebut" $ demandeAccesMesures_dateDebut x
            , maybe [] (schemaTypeToXML "dateFin") $ demandeAccesMesures_dateFin x
            , schemaTypeToXML "declarationAccordClient" $ demandeAccesMesures_declarationAccordClient x
            , schemaTypeToXML "mesuresTypeCode" $ demandeAccesMesures_mesuresTypeCode x
            , schemaTypeToXML "soutirage" $ demandeAccesMesures_soutirage x
            , schemaTypeToXML "injection" $ demandeAccesMesures_injection x
            , maybe [] (schemaTypeToXML "mesuresPas") $ demandeAccesMesures_mesuresPas x
            , maybe [] (schemaTypeToXML "mesuresCorrigees") $ demandeAccesMesures_mesuresCorrigees x
            , schemaTypeToXML "transmissionRecurrente" $ demandeAccesMesures_transmissionRecurrente x
            , maybe [] (schemaTypeToXML "periodiciteTransmission") $ demandeAccesMesures_periodiciteTransmission x
            ]
 
data DemandeType = DemandeType
        { demandeType_donneesGenerales :: DonneesGeneralesType
        , demandeType_accesMesures :: DemandeAccesMesures
        }
        deriving (Eq,Show)
instance SchemaType DemandeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandeType
            `apply` parseSchemaType "donneesGenerales"
            `apply` parseSchemaType "accesMesures"
    schemaTypeToXML s x@DemandeType{} =
        toXMLElement s []
            [ schemaTypeToXML "donneesGenerales" $ demandeType_donneesGenerales x
            , schemaTypeToXML "accesMesures" $ demandeType_accesMesures x
            ]
 
data DonneesGeneralesType = DonneesGeneralesType
        { donneesGeneralesType_refExterne :: Maybe Ds.Chaine255Type
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
            `apply` optional (parseSchemaType "refExterne")
            `apply` parseSchemaType "objetCode"
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "initiateurLogin"
            `apply` parseSchemaType "contratId"
    schemaTypeToXML s x@DonneesGeneralesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "refExterne") $ donneesGeneralesType_refExterne x
            , schemaTypeToXML "objetCode" $ donneesGeneralesType_objetCode x
            , schemaTypeToXML "pointId" $ donneesGeneralesType_pointId x
            , schemaTypeToXML "initiateurLogin" $ donneesGeneralesType_initiateurLogin x
            , schemaTypeToXML "contratId" $ donneesGeneralesType_contratId x
            ]
 
newtype PersonneMoraleType = PersonneMoraleType
        { personneMoraleType_denominationSociale :: Ds.Chaine255Type
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
 
data PersonnePhysiqueType = PersonnePhysiqueType
        { personnePhysiqueType_civilite :: Maybe Ds.CiviliteAbreviationType
        , personnePhysiqueType_nom :: Ds.Chaine255Type
        , personnePhysiqueType_prenom :: Maybe Ds.Chaine255Type
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
