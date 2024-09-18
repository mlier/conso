{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Conso.Fr.Elec.Sge.RechercherPointV20Type
  ( module Conso.Fr.Elec.Sge.RechercherPointV20Type
  ) where
 
import Text.XML.HaXml.Schema.Schema as Schema
    ( between,
      getAttribute,
      toXMLAttribute,
      toXMLElement,
      interior,
      posnElement,
      optional,
      apply,
      SchemaType(..),
      Occurs(Occurs),
      Content,
      XMLParser,
      Commitment(commit) )
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50 as Dc
    ( StructureComptageType,
      PointEtatContractuelType,
      AdresseAfnorType )
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( UtilisateurLoginType,
      PointIdType,
      EtablissementNumSiretType,
      DomaineTensionCodeType,
      CommuneFranceCodeInseeType,
      CodePostalFrancaisType,
      ClientFinalCategorieCodeType,
      Chaine255Type,
      AdresseAfnorLigneType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementRechercherPoint :: XMLParser RechercherPointType
elementRechercherPoint = parseSchemaType "sc:rechercherPoint"
elementToXMLRechercherPoint :: RechercherPointType -> [Content ()]
elementToXMLRechercherPoint = schemaTypeToXML "sc:rechercherPoint"
 
elementRechercherPointResponse :: XMLParser RechercherPointResponseType
elementRechercherPointResponse = parseSchemaType "ns1:rechercherPointResponse"
elementToXMLRechercherPointResponse :: RechercherPointResponseType -> [Content ()]
elementToXMLRechercherPointResponse = schemaTypeToXML "ns1:rechercherPointResponse"
 
data RechercherPointType = RechercherPointType
        { rechercherPointType_criteres :: CriteresType
        , rechercherPointType_loginUtilisateur :: Ds.UtilisateurLoginType
        }
        deriving (Eq,Show)
instance SchemaType RechercherPointType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RechercherPointType
            `apply` parseSchemaType "criteres"
            `apply` parseSchemaType "loginUtilisateur"
    schemaTypeToXML s x@RechercherPointType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/services/rechercherpoint/v2.0" 
                       ]
            [ schemaTypeToXML "criteres" $ rechercherPointType_criteres x
            , schemaTypeToXML "loginUtilisateur" $ rechercherPointType_loginUtilisateur x
            ]
 
newtype RechercherPointResponseType = RechercherPointResponseType
        { rechercherPointResponseType_points :: Maybe PointsType
        }
        deriving (Eq,Show)
instance SchemaType RechercherPointResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RechercherPointResponseType
            `apply` optional (parseSchemaType "points")
    schemaTypeToXML s x@RechercherPointResponseType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "points") $ rechercherPointResponseType_points x
            ]
 
newtype PointsType = PointsType
        { pointsType_point :: [PointType]
        }
        deriving (Eq,Show)
instance SchemaType PointsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PointsType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "point")
    schemaTypeToXML s x@PointsType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "point") $ pointsType_point x
            ]
 
data PointType = PointType
        { pointType_id :: Ds.PointIdType
        , pointType_adresseInstallationNormalisee :: Dc.AdresseAfnorType
        , pointType_matricule :: [Ds.Chaine255Type]
        , pointType_numeroSerie :: [Ds.Chaine255Type]
        , pointType_typeComptage :: Maybe Dc.StructureComptageType
        , pointType_etatContractuel :: Dc.PointEtatContractuelType
        , pointType_nomClientFinalOuDenominationSociale :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PointType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "id" e pos
        commit $ interior e $ return (PointType a0)
            `apply` parseSchemaType "adresseInstallationNormalisee"
            `apply` between (Occurs (Just 0) (Just 100))
                            (parseSchemaType "matricule")
            `apply` between (Occurs (Just 0) (Just 100))
                            (parseSchemaType "numeroSerie")
            `apply` optional (parseSchemaType "typeComptage")
            `apply` parseSchemaType "etatContractuel"
            `apply` optional (parseSchemaType "nomClientFinalOuDenominationSociale")
    schemaTypeToXML s x@PointType{} =
        toXMLElement s [ toXMLAttribute "id" $ pointType_id x
                       ]
            [ schemaTypeToXML "adresseInstallationNormalisee" $ pointType_adresseInstallationNormalisee x
            , concatMap (schemaTypeToXML "matricule") $ pointType_matricule x
            , concatMap (schemaTypeToXML "numeroSerie") $ pointType_numeroSerie x
            , maybe [] (schemaTypeToXML "typeComptage") $ pointType_typeComptage x
            , schemaTypeToXML "etatContractuel" $ pointType_etatContractuel x
            , maybe [] (schemaTypeToXML "nomClientFinalOuDenominationSociale") $ pointType_nomClientFinalOuDenominationSociale x
            ]
 
data CriteresType = CriteresType
        { criteresType_adresseInstallation :: Maybe AdresseInstallationType
        , criteresType_numSiret :: Maybe Ds.EtablissementNumSiretType
        , criteresType_matriculeOuNumeroSerie :: Maybe Ds.Chaine255Type
        , criteresType_domaineTensionAlimentationCode :: Maybe Ds.DomaineTensionCodeType
        , criteresType_nomClientFinalOuDenominationSociale :: Maybe Ds.Chaine255Type
        , criteresType_categorieClientFinalCode :: Maybe Ds.ClientFinalCategorieCodeType
        , criteresType_rechercheHorsPerimetre :: Maybe Xsd.Boolean
        }
        deriving (Eq,Show)
instance SchemaType CriteresType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CriteresType
            `apply` optional (parseSchemaType "adresseInstallation")
            `apply` optional (parseSchemaType "numSiret")
            `apply` optional (parseSchemaType "matriculeOuNumeroSerie")
            `apply` optional (parseSchemaType "domaineTensionAlimentationCode")
            `apply` optional (parseSchemaType "nomClientFinalOuDenominationSociale")
            `apply` optional (parseSchemaType "categorieClientFinalCode")
            `apply` optional (parseSchemaType "rechercheHorsPerimetre")
    schemaTypeToXML s x@CriteresType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "adresseInstallation") $ criteresType_adresseInstallation x
            , maybe [] (schemaTypeToXML "numSiret") $ criteresType_numSiret x
            , maybe [] (schemaTypeToXML "matriculeOuNumeroSerie") $ criteresType_matriculeOuNumeroSerie x
            , maybe [] (schemaTypeToXML "domaineTensionAlimentationCode") $ criteresType_domaineTensionAlimentationCode x
            , maybe [] (schemaTypeToXML "nomClientFinalOuDenominationSociale") $ criteresType_nomClientFinalOuDenominationSociale x
            , maybe [] (schemaTypeToXML "categorieClientFinalCode") $ criteresType_categorieClientFinalCode x
            , maybe [] (schemaTypeToXML "rechercheHorsPerimetre") $ criteresType_rechercheHorsPerimetre x
            ]
 
data AdresseInstallationType = AdresseInstallationType
        { adresseInstallationType_escalierEtEtageEtAppartement :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_batiment :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_numeroEtNomVoie :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_lieuDit :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_codePostal :: Maybe Ds.CodePostalFrancaisType
        , adresseInstallationType_codeInseeCommune :: Maybe Ds.CommuneFranceCodeInseeType
        }
        deriving (Eq,Show)
instance SchemaType AdresseInstallationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AdresseInstallationType
            `apply` optional (parseSchemaType "escalierEtEtageEtAppartement")
            `apply` optional (parseSchemaType "batiment")
            `apply` optional (parseSchemaType "numeroEtNomVoie")
            `apply` optional (parseSchemaType "lieuDit")
            `apply` optional (parseSchemaType "codePostal")
            `apply` optional (parseSchemaType "codeInseeCommune")
    schemaTypeToXML s x@AdresseInstallationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "escalierEtEtageEtAppartement") $ adresseInstallationType_escalierEtEtageEtAppartement x
            , maybe [] (schemaTypeToXML "batiment") $ adresseInstallationType_batiment x
            , maybe [] (schemaTypeToXML "numeroEtNomVoie") $ adresseInstallationType_numeroEtNomVoie x
            , maybe [] (schemaTypeToXML "lieuDit") $ adresseInstallationType_lieuDit x
            , maybe [] (schemaTypeToXML "codePostal") $ adresseInstallationType_codePostal x
            , maybe [] (schemaTypeToXML "codeInseeCommune") $ adresseInstallationType_codeInseeCommune x
            ]
