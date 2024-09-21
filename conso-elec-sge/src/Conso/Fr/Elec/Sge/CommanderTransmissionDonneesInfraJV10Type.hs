{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# LANGUAGE InstanceSigs #-}

module Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Type
  ( module Conso.Fr.Elec.Sge.CommanderTransmissionDonneesInfraJV10Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( SchemaType(..),
      XMLParser,
      Content,
      foldOneOf2,
      between,
      toXMLAttribute,
      toXMLElement,
      interior,
      posnElement,
      optional,
      apply,
      OneOf2(..),
      Occurs(Occurs),
      Commitment(commit, oneOf') )
      
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc
    ( PrestationOptionType, PrestationFicheType, PrestationCasType )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( UtilisateurLoginType,
      PointIdType,
      NbEntierType,
      DemandeObjetCodeType,
      ContratIdType,
      CiviliteAbreviationType,
      Chaine255Type(..),
      BooleenType,
      AffaireIdType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementCommanderTransmissionDonneesInfraJ :: XMLParser CommanderTransmissionDonneesInfraJType
elementCommanderTransmissionDonneesInfraJ = parseSchemaType "sc:commanderTransmissionDonneesInfraJ"
elementToXMLCommanderTransmissionDonneesInfraJ :: CommanderTransmissionDonneesInfraJType -> [Content ()]
elementToXMLCommanderTransmissionDonneesInfraJ = schemaTypeToXML "sc:commanderTransmissionDonneesInfraJ"
 
elementCommanderTransmissionDonneesInfraJResponse :: XMLParser CommanderTransmissionDonneesInfraJResponseType
elementCommanderTransmissionDonneesInfraJResponse = parseSchemaType "commanderTransmissionDonneesInfraJResponse"
elementToXMLCommanderTransmissionDonneesInfraJResponse :: CommanderTransmissionDonneesInfraJResponseType -> [Content ()]
elementToXMLCommanderTransmissionDonneesInfraJResponse = schemaTypeToXML "commanderTransmissionDonneesInfraJResponse"
 
data CommanderTransmissionDonneesInfraJResponseType = CommanderTransmissionDonneesInfraJResponseType
        { commanderTransmissionDonneesInfraJResponseType_demandeId :: Ds.AffaireIdType
        , commanderTransmissionDonneesInfraJResponseType_prestations :: Maybe PrestationsType
        }
        deriving (Eq,Show)
instance SchemaType CommanderTransmissionDonneesInfraJResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderTransmissionDonneesInfraJResponseType
            `apply` parseSchemaType "demandeId"
            `apply` optional (parseSchemaType "prestations")
    schemaTypeToXML s x@CommanderTransmissionDonneesInfraJResponseType{} =
        toXMLElement s []
            [ schemaTypeToXML "demandeId" $ commanderTransmissionDonneesInfraJResponseType_demandeId x
            , maybe [] (schemaTypeToXML "prestations") $ commanderTransmissionDonneesInfraJResponseType_prestations x
            ]
 
newtype CommanderTransmissionDonneesInfraJType = CommanderTransmissionDonneesInfraJType
        { commanderTransmissionDonneesInfraJType_demande :: DemandeType
        }
        deriving (Eq,Show)
instance SchemaType CommanderTransmissionDonneesInfraJType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CommanderTransmissionDonneesInfraJType
            `apply` parseSchemaType "demande"
    schemaTypeToXML s x@CommanderTransmissionDonneesInfraJType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/commandertransmissiondonneesinfraj/v1.0" 
                       ]
            [ schemaTypeToXML "demande" $ commanderTransmissionDonneesInfraJType_demande x
            ]
 
data DeclarationAccordClientType = DeclarationAccordClientType
        { declarationAccordClientType_accordClient :: Ds.BooleenType
        , declarationAccordClientType_injection :: Ds.BooleenType
        , declarationAccordClientType_soutirage :: Ds.BooleenType
        , declarationAccordClientType_choice3 :: OneOf2 PersonnePhysiqueType PersonneMoraleType
          -- ^ Choice between:
          --   
          --   (1) personnePhysique
          --   
          --   (2) personneMorale
        }
        deriving (Eq,Show)
instance SchemaType DeclarationAccordClientType where
    parseSchemaType :: String -> XMLParser DeclarationAccordClientType
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DeclarationAccordClientType
            `apply` parseSchemaType "accordClient"
            `apply` parseSchemaType "injection"
            `apply` parseSchemaType "soutirage"
            `apply` oneOf' [ ("PersonnePhysiqueType", fmap OneOf2 (parseSchemaType "personnePhysique"))
                           , ("PersonneMoraleType", fmap TwoOf2 (parseSchemaType "personneMorale"))
                           ]
    schemaTypeToXML s x@DeclarationAccordClientType{} =
        toXMLElement s []
            [ schemaTypeToXML "accordClient" $ declarationAccordClientType_accordClient x
            , schemaTypeToXML "injection" $ declarationAccordClientType_injection x
            , schemaTypeToXML "soutirage" $ declarationAccordClientType_soutirage x
            , foldOneOf2  (schemaTypeToXML "personnePhysique")
                          (schemaTypeToXML "personneMorale")
                          $ declarationAccordClientType_choice3 x
            ]
 
data DemandeAccesDonneesType = DemandeAccesDonneesType
        { demandeAccesDonneesType_declarationAccordClient :: [DeclarationAccordClientType]
        , demandeAccesDonneesType_injection :: Ds.BooleenType
        , demandeAccesDonneesType_soutirage :: Ds.BooleenType
        , demandeAccesDonneesType_cdc :: Ds.BooleenType
        , demandeAccesDonneesType_idx :: Ds.BooleenType
        , demandeAccesDonneesType_ptd :: Ds.BooleenType
        }
        deriving (Eq,Show)
instance SchemaType DemandeAccesDonneesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DemandeAccesDonneesType
            `apply` between (Occurs Nothing (Just 2))
                            (parseSchemaType "declarationAccordClient")
            `apply` parseSchemaType "injection"
            `apply` parseSchemaType "soutirage"
            `apply` parseSchemaType "cdc"
            `apply` parseSchemaType "idx"
            `apply` parseSchemaType "ptd"
    schemaTypeToXML s x@DemandeAccesDonneesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "declarationAccordClient") $ demandeAccesDonneesType_declarationAccordClient x
            , schemaTypeToXML "injection" $ demandeAccesDonneesType_injection x
            , schemaTypeToXML "soutirage" $ demandeAccesDonneesType_soutirage x
            , schemaTypeToXML "cdc" $ demandeAccesDonneesType_cdc x
            , schemaTypeToXML "idx" $ demandeAccesDonneesType_idx x
            , schemaTypeToXML "ptd" $ demandeAccesDonneesType_ptd x
            ]
 
data DemandeType = DemandeType
        { demandeType_donneesGenerales :: DonneesGeneralesType
        , demandeType_accesDonnees :: DemandeAccesDonneesType
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
