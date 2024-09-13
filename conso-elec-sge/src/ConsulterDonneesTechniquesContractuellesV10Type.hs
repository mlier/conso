{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module ConsulterDonneesTechniquesContractuellesV10Type
  ( module ConsulterDonneesTechniquesContractuellesV10Type
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
      Extension(..),
      SchemaType(..),
      Occurs(Occurs),
      Content,
      XMLParser,
      Commitment(commit) )
import Text.XML.HaXml.OneOfN ()
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xsd
import EnedisDictionnaireTypeComplexeV50 as Dc
    ( TransformateurPrecisionClasseType,
      TransformateurCourantPositionType,
      TransformateurCouplageType,
      TransformateurCalibreType,
      TensionLivraisonType,
      StructureTarifaireContexteUtilisationType,
      StructureComptageType,
      ReleveTraitementModeType,
      RelevePlageType,
      ReleveModeType,
      ReleveMediaType,
      PuissanceType,
      PointSegmentClienteleType,
      PointEtatContractuelType,
      PlageHeuresCreusesType,
      PeriodiciteType,
      EquipementElectriqueLocalisationType,
      DureeType,
      DomaineTensionType,
      DisjoncteurCalibreType,
      CommuneFranceType,
      ClasseTemporelleType(ClasseTemporelleType),
      CalendrierType,
      AlimentationModeApresCompteurType,
      AcheminementTarifType(AcheminementTarifType) )
import EnedisDictionnaireTypeSimpleV50 as Ds
    ( UtilisateurLoginType,
      PosteHoraireCodeType,
      PointIdType,
      NiveauOuvertureServicesCodeType,
      NbEntierType,
      LigneTelephoniqueNumType,
      HeureType,
      DateType,
      CodePostalFrancaisType,
      CleTeleAccesType,
      ClasseTemporelleCodeType,
      Chaine255Type,
      Chaine15Type,
      BooleenType,
      AdresseAfnorLigneType,
      AcheminementTarifCodeType )

-- Some hs-boot imports are required, for fwd-declaring types.

data AdresseInstallationType = AdresseInstallationType
        { adresseInstallationType_escalierEtEtageEtAppartement :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_batiment :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_numeroEtNomVoie :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_lieuDit :: Maybe Ds.AdresseAfnorLigneType
        , adresseInstallationType_codePostal :: Maybe Ds.CodePostalFrancaisType
        , adresseInstallationType_commune :: Dc.CommuneFranceType
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
            `apply` parseSchemaType "commune"
    schemaTypeToXML s x@AdresseInstallationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "escalierEtEtageEtAppartement") $ adresseInstallationType_escalierEtEtageEtAppartement x
            , maybe [] (schemaTypeToXML "batiment") $ adresseInstallationType_batiment x
            , maybe [] (schemaTypeToXML "numeroEtNomVoie") $ adresseInstallationType_numeroEtNomVoie x
            , maybe [] (schemaTypeToXML "lieuDit") $ adresseInstallationType_lieuDit x
            , maybe [] (schemaTypeToXML "codePostal") $ adresseInstallationType_codePostal x
            , schemaTypeToXML "commune" $ adresseInstallationType_commune x
            ]

data AlimentationPrincipaleType = AlimentationPrincipaleType
        { alimentationPrincipaleType_domaineTension :: Dc.DomaineTensionType
        , alimentationPrincipaleType_tensionLivraison :: Maybe Dc.TensionLivraisonType
        , alimentationPrincipaleType_modeApresCompteur :: Maybe Dc.AlimentationModeApresCompteurType
        , alimentationPrincipaleType_puissanceRaccordementSoutirage :: Maybe Dc.PuissanceType
        }
        deriving (Eq,Show)
instance SchemaType AlimentationPrincipaleType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AlimentationPrincipaleType
            `apply` parseSchemaType "domaineTension"
            `apply` optional (parseSchemaType "tensionLivraison")
            `apply` optional (parseSchemaType "modeApresCompteur")
            `apply` optional (parseSchemaType "puissanceRaccordementSoutirage")
    schemaTypeToXML s x@AlimentationPrincipaleType{} =
        toXMLElement s []
            [ schemaTypeToXML "domaineTension" $ alimentationPrincipaleType_domaineTension x
            , maybe [] (schemaTypeToXML "tensionLivraison") $ alimentationPrincipaleType_tensionLivraison x
            , maybe [] (schemaTypeToXML "modeApresCompteur") $ alimentationPrincipaleType_modeApresCompteur x
            , maybe [] (schemaTypeToXML "puissanceRaccordementSoutirage") $ alimentationPrincipaleType_puissanceRaccordementSoutirage x
            ]

newtype CompteursType = CompteursType
        { compteursType_compteur :: [CompteurType]
        }
        deriving (Eq,Show)
instance SchemaType CompteursType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CompteursType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "compteur")
    schemaTypeToXML s x@CompteursType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "compteur") $ compteursType_compteur x
            ]

data CompteurType = CompteurType
        { compteurType_localisation :: Maybe Dc.EquipementElectriqueLocalisationType
        , compteurType_matricule :: Maybe Ds.Chaine255Type
        , compteurType_ticActivee :: Maybe Ds.BooleenType
        , compteurType_ticStandard :: Maybe Ds.BooleenType
        , compteurType_ticActivable :: Maybe Ds.BooleenType
        , compteurType_plagesHeuresCreuses :: Maybe Ds.Chaine255Type
        , compteurType_parametresTeleAcces :: Maybe ParametresTeleAccesType
        , compteurType_programmationHoraire :: Maybe ProgrammationHoraireType
        }
        deriving (Eq,Show)
instance SchemaType CompteurType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CompteurType
            `apply` optional (parseSchemaType "localisation")
            `apply` optional (parseSchemaType "matricule")
            `apply` optional (parseSchemaType "ticActivee")
            `apply` optional (parseSchemaType "ticStandard")
            `apply` optional (parseSchemaType "ticActivable")
            `apply` optional (parseSchemaType "plagesHeuresCreuses")
            `apply` optional (parseSchemaType "parametresTeleAcces")
            `apply` optional (parseSchemaType "programmationHoraire")
    schemaTypeToXML s x@CompteurType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "localisation") $ compteurType_localisation x
            , maybe [] (schemaTypeToXML "matricule") $ compteurType_matricule x
            , maybe [] (schemaTypeToXML "ticActivee") $ compteurType_ticActivee x
            , maybe [] (schemaTypeToXML "ticStandard") $ compteurType_ticStandard x
            , maybe [] (schemaTypeToXML "ticActivable") $ compteurType_ticActivable x
            , maybe [] (schemaTypeToXML "plagesHeuresCreuses") $ compteurType_plagesHeuresCreuses x
            , maybe [] (schemaTypeToXML "parametresTeleAcces") $ compteurType_parametresTeleAcces x
            , maybe [] (schemaTypeToXML "programmationHoraire") $ compteurType_programmationHoraire x
            ]
instance Extension CompteurType EquipementElectriqueType where
    supertype (CompteurType e0 _ _ _ _ _ _ _) =
               EquipementElectriqueType e0

elementConsulterDonneesTechniquesContractuelles :: XMLParser ConsulterDonneesTechniquesContractuellesType
elementConsulterDonneesTechniquesContractuelles = parseSchemaType "sc:consulterDonneesTechniquesContractuelles"
elementToXMLConsulterDonneesTechniquesContractuelles :: ConsulterDonneesTechniquesContractuellesType -> [Content ()]
elementToXMLConsulterDonneesTechniquesContractuelles = schemaTypeToXML "sc:consulterDonneesTechniquesContractuelles"

elementConsulterDonneesTechniquesContractuellesResponse :: XMLParser ConsulterDonneesTechniquesContractuellesResponseType
elementConsulterDonneesTechniquesContractuellesResponse = parseSchemaType "ns7:consulterDonneesTechniquesContractuellesResponse"
elementToXMLConsulterDonneesTechniquesContractuellesResponse :: ConsulterDonneesTechniquesContractuellesResponseType -> [Content ()]
elementToXMLConsulterDonneesTechniquesContractuellesResponse = schemaTypeToXML "ns7:consulterDonneesTechniquesContractuellesResponse"

newtype ConsulterDonneesTechniquesContractuellesResponseType = ConsulterDonneesTechniquesContractuellesResponseType
        { consulterDonneesTechniquesContractuellesResponseType_point :: PointType
        }
        deriving (Eq,Show)
instance SchemaType ConsulterDonneesTechniquesContractuellesResponseType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ConsulterDonneesTechniquesContractuellesResponseType
            `apply` parseSchemaType "point"
    schemaTypeToXML s x@ConsulterDonneesTechniquesContractuellesResponseType{} =
        toXMLElement s []
            [ schemaTypeToXML "point" $ consulterDonneesTechniquesContractuellesResponseType_point x
            ]

data ConsulterDonneesTechniquesContractuellesType = ConsulterDonneesTechniquesContractuellesType
        { consulterDonneesTechniquesContractuellesType_pointId :: Ds.PointIdType
        , consulterDonneesTechniquesContractuellesType_loginUtilisateur :: Ds.UtilisateurLoginType
        , consulterDonneesTechniquesContractuellesType_autorisationClient :: Maybe Ds.BooleenType
        }
        deriving (Eq,Show)
instance SchemaType ConsulterDonneesTechniquesContractuellesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ConsulterDonneesTechniquesContractuellesType
            `apply` parseSchemaType "pointId"
            `apply` parseSchemaType "loginUtilisateur"
            `apply` optional (parseSchemaType "autorisationClient")
    schemaTypeToXML s x@ConsulterDonneesTechniquesContractuellesType{} =
        toXMLElement s [ toXMLAttribute "xmlns:sc" $ Xsd.XsdString "http://www.enedis.fr/sge/b2b/services/consulterdonneestechniquescontractuelles/v1.0" 
                       ]
            [ schemaTypeToXML "pointId" $ consulterDonneesTechniquesContractuellesType_pointId x
            , schemaTypeToXML "loginUtilisateur" $ consulterDonneesTechniquesContractuellesType_loginUtilisateur x
            , maybe [] (schemaTypeToXML "autorisationClient") $ consulterDonneesTechniquesContractuellesType_autorisationClient x
            ]

newtype DenivelePuissancesClassesTemporellesType = DenivelePuissancesClassesTemporellesType
        { denivelePuissancesClassesTemporellesType_classeTemporelle :: [DenivelePuissancesClasseTemporelleType]
        }
        deriving (Eq,Show)
instance SchemaType DenivelePuissancesClassesTemporellesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DenivelePuissancesClassesTemporellesType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "classeTemporelle")
    schemaTypeToXML s x@DenivelePuissancesClassesTemporellesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "classeTemporelle") $ denivelePuissancesClassesTemporellesType_classeTemporelle x
            ]

data DenivelePuissancesClasseTemporelleType = DenivelePuissancesClasseTemporelleType
        { denivelePuissancesClasseTemporelleType_code :: Ds.ClasseTemporelleCodeType
        , denivelePuissancesClasseTemporelleType_libelle :: Maybe Ds.Chaine255Type
        , denivelePuissancesClasseTemporelleType_puissance :: Dc.PuissanceType
        }
        deriving (Eq,Show)
instance SchemaType DenivelePuissancesClasseTemporelleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DenivelePuissancesClasseTemporelleType a0)
            `apply` optional (parseSchemaType "libelle")
            `apply` parseSchemaType "puissance"
    schemaTypeToXML s x@DenivelePuissancesClasseTemporelleType{} =
        toXMLElement s [ toXMLAttribute "code" $ denivelePuissancesClasseTemporelleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ denivelePuissancesClasseTemporelleType_libelle x
            , schemaTypeToXML "puissance" $ denivelePuissancesClasseTemporelleType_puissance x
            ]
instance Extension DenivelePuissancesClasseTemporelleType Dc.ClasseTemporelleType where
    supertype (DenivelePuissancesClasseTemporelleType a0 e0 _) =
               ClasseTemporelleType a0 e0

newtype DisjoncteurType = DisjoncteurType
        { disjoncteurType_calibre :: Maybe Dc.DisjoncteurCalibreType
        }
        deriving (Eq,Show)
instance SchemaType DisjoncteurType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DisjoncteurType
            `apply` optional (parseSchemaType "calibre")
    schemaTypeToXML s x@DisjoncteurType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "calibre") $ disjoncteurType_calibre x
            ]

data DispositifComptageType = DispositifComptageType
        { dispositifComptageType_typeComptage :: Dc.StructureComptageType
        , dispositifComptageType_compteurs :: Maybe CompteursType
        , dispositifComptageType_disjoncteur :: Maybe DisjoncteurType
        , dispositifComptageType_relais :: Maybe RelaisType
        , dispositifComptageType_transformateurCourant :: Maybe TransformateurCourantType
        , dispositifComptageType_transformateurTension :: Maybe TransformateurTensionType
        }
        deriving (Eq,Show)
instance SchemaType DispositifComptageType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DispositifComptageType
            `apply` parseSchemaType "typeComptage"
            `apply` optional (parseSchemaType "compteurs")
            `apply` optional (parseSchemaType "disjoncteur")
            `apply` optional (parseSchemaType "relais")
            `apply` optional (parseSchemaType "transformateurCourant")
            `apply` optional (parseSchemaType "transformateurTension")
    schemaTypeToXML s x@DispositifComptageType{} =
        toXMLElement s []
            [ schemaTypeToXML "typeComptage" $ dispositifComptageType_typeComptage x
            , maybe [] (schemaTypeToXML "compteurs") $ dispositifComptageType_compteurs x
            , maybe [] (schemaTypeToXML "disjoncteur") $ dispositifComptageType_disjoncteur x
            , maybe [] (schemaTypeToXML "relais") $ dispositifComptageType_relais x
            , maybe [] (schemaTypeToXML "transformateurCourant") $ dispositifComptageType_transformateurCourant x
            , maybe [] (schemaTypeToXML "transformateurTension") $ dispositifComptageType_transformateurTension x
            ]

newtype EquipementElectriqueType = EquipementElectriqueType
        { equipementElectriqueType_localisation :: Maybe Dc.EquipementElectriqueLocalisationType
        }
        deriving (Eq,Show)
instance SchemaType EquipementElectriqueType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return EquipementElectriqueType
            `apply` optional (parseSchemaType "localisation")
    schemaTypeToXML s x@EquipementElectriqueType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "localisation") $ equipementElectriqueType_localisation x
            ]

data FenetreEcouteClientType = FenetreEcouteClientType
        { fenetreEcouteClientType_heureDebut :: Maybe Ds.HeureType
        , fenetreEcouteClientType_duree :: Maybe Dc.DureeType
        }
        deriving (Eq,Show)
instance SchemaType FenetreEcouteClientType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return FenetreEcouteClientType
            `apply` optional (parseSchemaType "heureDebut")
            `apply` optional (parseSchemaType "duree")
    schemaTypeToXML s x@FenetreEcouteClientType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "heureDebut") $ fenetreEcouteClientType_heureDebut x
            , maybe [] (schemaTypeToXML "duree") $ fenetreEcouteClientType_duree x
            ]

data FormuleTarifaireAcheminementEtPostesHorairesType = FormuleTarifaireAcheminementEtPostesHorairesType
        { formuleTarifaireAcheminementEtPostesHorairesType_code :: Ds.AcheminementTarifCodeType
        , formuleTarifaireAcheminementEtPostesHorairesType_libelle :: Maybe Ds.Chaine255Type
        , formuleTarifaireAcheminementEtPostesHorairesType_programmationHoraire :: Maybe ProgrammationHoraireType
        }
        deriving (Eq,Show)
instance SchemaType FormuleTarifaireAcheminementEtPostesHorairesType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FormuleTarifaireAcheminementEtPostesHorairesType a0)
            `apply` optional (parseSchemaType "libelle")
            `apply` optional (parseSchemaType "programmationHoraire")
    schemaTypeToXML s x@FormuleTarifaireAcheminementEtPostesHorairesType{} =
        toXMLElement s [ toXMLAttribute "code" $ formuleTarifaireAcheminementEtPostesHorairesType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ formuleTarifaireAcheminementEtPostesHorairesType_libelle x
            , maybe [] (schemaTypeToXML "programmationHoraire") $ formuleTarifaireAcheminementEtPostesHorairesType_programmationHoraire x
            ]
instance Extension FormuleTarifaireAcheminementEtPostesHorairesType Dc.AcheminementTarifType where
    supertype (FormuleTarifaireAcheminementEtPostesHorairesType a0 e0 _) =
               AcheminementTarifType a0 e0

newtype FuturesProgrammationsHorairesType = FuturesProgrammationsHorairesType
        { futuresProgrammationsHorairesType_formuleTarifaireAcheminement :: [FormuleTarifaireAcheminementEtPostesHorairesType]
        }
        deriving (Eq,Show)
instance SchemaType FuturesProgrammationsHorairesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return FuturesProgrammationsHorairesType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "formuleTarifaireAcheminement")
    schemaTypeToXML s x@FuturesProgrammationsHorairesType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "formuleTarifaireAcheminement") $ futuresProgrammationsHorairesType_formuleTarifaireAcheminement x
            ]

data LongueUtilisationType = LongueUtilisationType
        { longueUtilisationType_contexte :: Dc.StructureTarifaireContexteUtilisationType
        , longueUtilisationType_forfait :: Maybe Dc.DureeType
        }
        deriving (Eq,Show)
instance SchemaType LongueUtilisationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return LongueUtilisationType
            `apply` parseSchemaType "contexte"
            `apply` optional (parseSchemaType "forfait")
    schemaTypeToXML s x@LongueUtilisationType{} =
        toXMLElement s []
            [ schemaTypeToXML "contexte" $ longueUtilisationType_contexte x
            , maybe [] (schemaTypeToXML "forfait") $ longueUtilisationType_forfait x
            ]

data ModalitesReleveType = ModalitesReleveType
        { modalitesReleveType_modeTraitement :: Maybe Dc.ReleveTraitementModeType
        , modalitesReleveType_periodicite :: Maybe Dc.PeriodiciteType
        , modalitesReleveType_plageReleve :: Maybe Dc.RelevePlageType
        }
        deriving (Eq,Show)
instance SchemaType ModalitesReleveType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ModalitesReleveType
            `apply` optional (parseSchemaType "modeTraitement")
            `apply` optional (parseSchemaType "periodicite")
            `apply` optional (parseSchemaType "plageReleve")
    schemaTypeToXML s x@ModalitesReleveType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "modeTraitement") $ modalitesReleveType_modeTraitement x
            , maybe [] (schemaTypeToXML "periodicite") $ modalitesReleveType_periodicite x
            , maybe [] (schemaTypeToXML "plageReleve") $ modalitesReleveType_plageReleve x
            ]

data ParametresTeleAccesType = ParametresTeleAccesType
        { parametresTeleAccesType_numeroTelephone :: Maybe Ds.LigneTelephoniqueNumType
        , parametresTeleAccesType_numeroVoieAiguillage :: Maybe Ds.NbEntierType
        , parametresTeleAccesType_etatLigneTelephonique :: Maybe Ds.Chaine15Type
        , parametresTeleAccesType_fenetreEcouteClient :: Maybe FenetreEcouteClientType
        , parametresTeleAccesType_cle :: Maybe Ds.CleTeleAccesType
        }
        deriving (Eq,Show)
instance SchemaType ParametresTeleAccesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ParametresTeleAccesType
            `apply` optional (parseSchemaType "numeroTelephone")
            `apply` optional (parseSchemaType "numeroVoieAiguillage")
            `apply` optional (parseSchemaType "etatLigneTelephonique")
            `apply` optional (parseSchemaType "fenetreEcouteClient")
            `apply` optional (parseSchemaType "cle")
    schemaTypeToXML s x@ParametresTeleAccesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "numeroTelephone") $ parametresTeleAccesType_numeroTelephone x
            , maybe [] (schemaTypeToXML "numeroVoieAiguillage") $ parametresTeleAccesType_numeroVoieAiguillage x
            , maybe [] (schemaTypeToXML "etatLigneTelephonique") $ parametresTeleAccesType_etatLigneTelephonique x
            , maybe [] (schemaTypeToXML "fenetreEcouteClient") $ parametresTeleAccesType_fenetreEcouteClient x
            , maybe [] (schemaTypeToXML "cle") $ parametresTeleAccesType_cle x
            ]

data PointDonneesGeneralesType = PointDonneesGeneralesType
        { pointDonneesGeneralesType_etatContractuel :: Dc.PointEtatContractuelType
        , pointDonneesGeneralesType_adresseInstallation :: AdresseInstallationType
        , pointDonneesGeneralesType_dateDerniereModificationFormuleTarifaireAcheminement :: Maybe Ds.DateType
        , pointDonneesGeneralesType_dateDerniereAugmentationPuissanceSouscrite :: Maybe Ds.DateType
        , pointDonneesGeneralesType_dateDerniereDiminutionPuissanceSouscrite :: Maybe Ds.DateType
        , pointDonneesGeneralesType_segment :: Maybe Dc.PointSegmentClienteleType
        , pointDonneesGeneralesType_niveauOuvertureServices :: Maybe Ds.NiveauOuvertureServicesCodeType
        }
        deriving (Eq,Show)
instance SchemaType PointDonneesGeneralesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PointDonneesGeneralesType
            `apply` parseSchemaType "etatContractuel"
            `apply` parseSchemaType "adresseInstallation"
            `apply` optional (parseSchemaType "dateDerniereModificationFormuleTarifaireAcheminement")
            `apply` optional (parseSchemaType "dateDerniereAugmentationPuissanceSouscrite")
            `apply` optional (parseSchemaType "dateDerniereDiminutionPuissanceSouscrite")
            `apply` optional (parseSchemaType "segment")
            `apply` optional (parseSchemaType "niveauOuvertureServices")
    schemaTypeToXML s x@PointDonneesGeneralesType{} =
        toXMLElement s []
            [ schemaTypeToXML "etatContractuel" $ pointDonneesGeneralesType_etatContractuel x
            , schemaTypeToXML "adresseInstallation" $ pointDonneesGeneralesType_adresseInstallation x
            , maybe [] (schemaTypeToXML "dateDerniereModificationFormuleTarifaireAcheminement") $ pointDonneesGeneralesType_dateDerniereModificationFormuleTarifaireAcheminement x
            , maybe [] (schemaTypeToXML "dateDerniereAugmentationPuissanceSouscrite") $ pointDonneesGeneralesType_dateDerniereAugmentationPuissanceSouscrite x
            , maybe [] (schemaTypeToXML "dateDerniereDiminutionPuissanceSouscrite") $ pointDonneesGeneralesType_dateDerniereDiminutionPuissanceSouscrite x
            , maybe [] (schemaTypeToXML "segment") $ pointDonneesGeneralesType_segment x
            , maybe [] (schemaTypeToXML "niveauOuvertureServices") $ pointDonneesGeneralesType_niveauOuvertureServices x
            ]

data PointType = PointType
        { pointType_id :: Ds.PointIdType
        , pointType_donneesGenerales :: PointDonneesGeneralesType
        , pointType_situationAlimentation :: Maybe SituationAlimentationType
        , pointType_situationComptage :: Maybe SituationComptageType
        , pointType_situationContractuelle :: Maybe SituationContractuelleType
        }
        deriving (Eq,Show)
instance SchemaType PointType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "id" e pos
        commit $ interior e $ return (PointType a0)
            `apply` parseSchemaType "donneesGenerales"
            `apply` optional (parseSchemaType "situationAlimentation")
            `apply` optional (parseSchemaType "situationComptage")
            `apply` optional (parseSchemaType "situationContractuelle")
    schemaTypeToXML s x@PointType{} =
        toXMLElement s [ toXMLAttribute "id" $ pointType_id x
                       ]
            [ schemaTypeToXML "donneesGenerales" $ pointType_donneesGenerales x
            , maybe [] (schemaTypeToXML "situationAlimentation") $ pointType_situationAlimentation x
            , maybe [] (schemaTypeToXML "situationComptage") $ pointType_situationComptage x
            , maybe [] (schemaTypeToXML "situationContractuelle") $ pointType_situationContractuelle x
            ]

newtype ProgrammationHoraireType = ProgrammationHoraireType
        { programmationHoraireType_programmationPosteHoraire :: [ProgrammationPosteHoraireType]
        }
        deriving (Eq,Show)
instance SchemaType ProgrammationHoraireType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return ProgrammationHoraireType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "programmationPosteHoraire")
    schemaTypeToXML s x@ProgrammationHoraireType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "programmationPosteHoraire") $ programmationHoraireType_programmationPosteHoraire x
            ]

data ProgrammationPosteHoraireType = ProgrammationPosteHoraireType
        { programmationPosteHoraireType_code :: Ds.PosteHoraireCodeType
        , programmationPosteHoraireType_libelle :: Maybe Ds.Chaine255Type
        , programmationPosteHoraireType_periodesHoraires :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ProgrammationPosteHoraireType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ProgrammationPosteHoraireType a0)
            `apply` optional (parseSchemaType "libelle")
            `apply` optional (parseSchemaType "periodesHoraires")
    schemaTypeToXML s x@ProgrammationPosteHoraireType{} =
        toXMLElement s [ toXMLAttribute "code" $ programmationPosteHoraireType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ programmationPosteHoraireType_libelle x
            , maybe [] (schemaTypeToXML "periodesHoraires") $ programmationPosteHoraireType_periodesHoraires x
            ]

newtype RelaisType = RelaisType
        { relaisType_plageHeuresCreuses :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RelaisType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return RelaisType
            `apply` optional (parseSchemaType "plageHeuresCreuses")
    schemaTypeToXML s x@RelaisType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "plageHeuresCreuses") $ relaisType_plageHeuresCreuses x
            ]

newtype SituationAlimentationType = SituationAlimentationType
        { situationAlimentationType_alimentationPrincipale :: Maybe AlimentationPrincipaleType
        }
        deriving (Eq,Show)
instance SchemaType SituationAlimentationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return SituationAlimentationType
            `apply` optional (parseSchemaType "alimentationPrincipale")
    schemaTypeToXML s x@SituationAlimentationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "alimentationPrincipale") $ situationAlimentationType_alimentationPrincipale x
            ]

data SituationComptageType = SituationComptageType
        { situationComptageType_dispositifComptage :: Maybe DispositifComptageType
        , situationComptageType_caracteristiquesReleve :: Maybe ModalitesReleveType
        , situationComptageType_modeReleve :: Maybe Dc.ReleveModeType
        , situationComptageType_mediaReleve :: Maybe Dc.ReleveMediaType
        , situationComptageType_futuresPlagesHeuresCreuses :: Maybe Dc.PlageHeuresCreusesType
        , situationComptageType_futuresProgrammationsHoraires :: Maybe FuturesProgrammationsHorairesType
        }
        deriving (Eq,Show)
instance SchemaType SituationComptageType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return SituationComptageType
            `apply` optional (parseSchemaType "dispositifComptage")
            `apply` optional (parseSchemaType "caracteristiquesReleve")
            `apply` optional (parseSchemaType "modeReleve")
            `apply` optional (parseSchemaType "mediaReleve")
            `apply` optional (parseSchemaType "futuresPlagesHeuresCreuses")
            `apply` optional (parseSchemaType "futuresProgrammationsHoraires")
    schemaTypeToXML s x@SituationComptageType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "dispositifComptage") $ situationComptageType_dispositifComptage x
            , maybe [] (schemaTypeToXML "caracteristiquesReleve") $ situationComptageType_caracteristiquesReleve x
            , maybe [] (schemaTypeToXML "modeReleve") $ situationComptageType_modeReleve x
            , maybe [] (schemaTypeToXML "mediaReleve") $ situationComptageType_mediaReleve x
            , maybe [] (schemaTypeToXML "futuresPlagesHeuresCreuses") $ situationComptageType_futuresPlagesHeuresCreuses x
            , maybe [] (schemaTypeToXML "futuresProgrammationsHoraires") $ situationComptageType_futuresProgrammationsHoraires x
            ]

newtype SituationContractuelleType = SituationContractuelleType
        { situationContractuelleType_structureTarifaire :: Maybe StructureTarifaireType
        }
        deriving (Eq,Show)
instance SchemaType SituationContractuelleType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return SituationContractuelleType
            `apply` optional (parseSchemaType "structureTarifaire")
    schemaTypeToXML s x@SituationContractuelleType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "structureTarifaire") $ situationContractuelleType_structureTarifaire x
            ]

newtype StructureTarifaireDenivelePuissancesType = StructureTarifaireDenivelePuissancesType
        { structureTarifaireDenivelePuissancesType_classesTemporelles :: Maybe DenivelePuissancesClassesTemporellesType
        }
        deriving (Eq,Show)
instance SchemaType StructureTarifaireDenivelePuissancesType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return StructureTarifaireDenivelePuissancesType
            `apply` optional (parseSchemaType "classesTemporelles")
    schemaTypeToXML s x@StructureTarifaireDenivelePuissancesType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "classesTemporelles") $ structureTarifaireDenivelePuissancesType_classesTemporelles x
            ]

data StructureTarifaireType = StructureTarifaireType
        { structureTarifaireType_formuleTarifaireAcheminement :: Maybe Dc.AcheminementTarifType
        , structureTarifaireType_longueUtilisation :: Maybe LongueUtilisationType
        , structureTarifaireType_puissanceSouscriteMax :: Maybe Dc.PuissanceType
        , structureTarifaireType_denivelePuissances :: Maybe StructureTarifaireDenivelePuissancesType
        , structureTarifaireType_calendrierFrn :: Maybe Dc.CalendrierType
        }
        deriving (Eq,Show)
instance SchemaType StructureTarifaireType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return StructureTarifaireType
            `apply` optional (parseSchemaType "formuleTarifaireAcheminement")
            `apply` optional (parseSchemaType "longueUtilisation")
            `apply` optional (parseSchemaType "puissanceSouscriteMax")
            `apply` optional (parseSchemaType "denivelePuissances")
            `apply` optional (parseSchemaType "calendrierFrn")
    schemaTypeToXML s x@StructureTarifaireType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "formuleTarifaireAcheminement") $ structureTarifaireType_formuleTarifaireAcheminement x
            , maybe [] (schemaTypeToXML "longueUtilisation") $ structureTarifaireType_longueUtilisation x
            , maybe [] (schemaTypeToXML "puissanceSouscriteMax") $ structureTarifaireType_puissanceSouscriteMax x
            , maybe [] (schemaTypeToXML "denivelePuissances") $ structureTarifaireType_denivelePuissances x
            , maybe [] (schemaTypeToXML "calendrierFrn") $ structureTarifaireType_calendrierFrn x
            ]

data TransformateurCourantType = TransformateurCourantType
        { transformateurCourantType_calibre :: Maybe Dc.TransformateurCalibreType
        , transformateurCourantType_couplage :: Maybe Dc.TransformateurCouplageType
        , transformateurCourantType_classePrecision :: Maybe Dc.TransformateurPrecisionClasseType
        , transformateurCourantType_position :: Maybe Dc.TransformateurCourantPositionType
        }
        deriving (Eq,Show)
instance SchemaType TransformateurCourantType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return TransformateurCourantType
            `apply` optional (parseSchemaType "calibre")
            `apply` optional (parseSchemaType "couplage")
            `apply` optional (parseSchemaType "classePrecision")
            `apply` optional (parseSchemaType "position")
    schemaTypeToXML s x@TransformateurCourantType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "calibre") $ transformateurCourantType_calibre x
            , maybe [] (schemaTypeToXML "couplage") $ transformateurCourantType_couplage x
            , maybe [] (schemaTypeToXML "classePrecision") $ transformateurCourantType_classePrecision x
            , maybe [] (schemaTypeToXML "position") $ transformateurCourantType_position x
            ]

data TransformateurTensionType = TransformateurTensionType
        { transformateurTensionType_calibre :: Maybe Dc.TransformateurCalibreType
        , transformateurTensionType_couplage :: Maybe Dc.TransformateurCouplageType
        , transformateurTensionType_classePrecision :: Maybe Dc.TransformateurPrecisionClasseType
        }
        deriving (Eq,Show)
instance SchemaType TransformateurTensionType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return TransformateurTensionType
            `apply` optional (parseSchemaType "calibre")
            `apply` optional (parseSchemaType "couplage")
            `apply` optional (parseSchemaType "classePrecision")
    schemaTypeToXML s x@TransformateurTensionType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "calibre") $ transformateurTensionType_calibre x
            , maybe [] (schemaTypeToXML "couplage") $ transformateurTensionType_couplage x
            , maybe [] (schemaTypeToXML "classePrecision") $ transformateurTensionType_classePrecision x
            ]
