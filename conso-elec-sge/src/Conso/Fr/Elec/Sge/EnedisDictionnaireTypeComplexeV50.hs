{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50
  ( module Conso.Fr.Elec.Sge.EnedisDictionnaireTypeComplexeV50
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
      Commitment(commit) )
import Text.XML.HaXml.OneOfN ()
import Conso.Fr.Elec.Sge.EnedisDictionnaireTypeSimpleV50 as Ds
    ( ZoneQualiteDesserteCodeType,
      UsageChantierCodeType,
      TransformateurPrecisionClasseCodeType,
      TransformateurCourantPositionCodeType,
      TransformateurCouplageTypeCodeType,
      TransformateurCalibreCodeType,
      TourneeCodeType,
      TensionUniteSymboleType,
      TensionLivraisonCodeType,
      StructureTarifaireContexteUtilisationCodeType,
      StructureComptageCodeType,
      SituationContractuelleGestionModeCodeType,
      SecteurGeographiqueCodeType,
      ResidenceTypeCodeType,
      ReleveTraitementModeCodeType,
      ReleveQualificationCodeType,
      RelevePlageCodeType,
      ReleveModeCodeType,
      ReleveMediaCodeType,
      RelaisNatureCodeType,
      RelaisCommandeTypeCodeType,
      RectificationMotifCodeType,
      ReclamationTypeCodeType,
      ReclamationSousTypeCodeType,
      RecevabiliteResultatCodeType,
      RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType,
      RecevabiliteMotifRefusCodeType,
      RattachementTypeCodeType,
      RattachementPointRoleCodeType,
      PuissanceUniteSymboleType,
      ProgrammeCircuitEauTempoCodeType,
      ProgrammeCircuitChauffageTempoCodeType,
      ProductionAutonomeCouplageModeCodeType,
      PrestationOptionCodeType,
      PrestationMotifNonRealisationCodeType,
      PrestationMotifNonFacturationCodeType,
      PrestationFicheCodeType,
      PrestationCasCodeType,
      PrejudiceNatureCodeType,
      PointSegmentClienteleCodeType,
      PointEtatContractuelCodeType,
      PointAppartenanceRegroupementTurpeCodeType,
      PointAppartenanceRegroupementHebergeurDecomptantCodeType,
      PlageHeuresCreusesCodeType,
      PeriodiciteCodeType,
      OptionBilanContinuiteFournitureTypeCodeType,
      OptionBilanContinuiteFournitureCoupureTypeCodeType,
      OperationCodeType,
      OffreTypeCodeType,
      NonPriseEnCompteAutoreleveMotifCodeType,
      NbEntierType,
      NbDecimalType,
      MesureTypeCodeType,
      MesureStatutCodeType,
      MesureOrigineCodeType,
      MesureNatureCodeType,
      MesureDeclencheurCodeType,
      LongueurUniteSymboleType,
      LimiteurTypeCodeType,
      InterventionReplanificationMotifCodeType,
      InterventionRealisationEtatCodeType,
      InterventionPlanificationHorsDelaiMotifCodeType,
      InterventionPeriodeTypeCodeType,
      InterventionNonRealisationMotifCodeType,
      InterventionEtatCodeType,
      InterventionDeplanificationMotifCodeType,
      InterventionDemandeMotifRefusCodeType,
      IntervenantTypeCodeType,
      IntensiteUniteSymboleType,
      GroupePeriodeMobileCodeType,
      GrandeurPhysiqueCodeType,
      FraudeOuDysfonctionnementMethodeFacturationCodeType,
      FraudeOuDysfonctionnementMethodeCalculCodeType,
      FraudeNatureCodeType,
      FraisCodeType,
      FinaliteCodeType,
      FaisabiliteReserveMotifCodeType,
      FaisabiliteMotifImpossibiliteCodeType,
      EquipementElectriqueRegimeProprieteCodeType,
      EquipementElectriqueLocalisationCodeType,
      DysfonctionnementNatureCodeType,
      DureeUniteSymboleType,
      DomaineTensionCodeType,
      DispositifParticulierLimitationPerturbationsCodeType,
      DispositifComptageParticulariteCodeType,
      DisjoncteurNatureCodeType,
      DisjoncteurCalibreCodeType,
      DeviseAbreviationType,
      DemandeTechniqueTypeCodeType,
      DemandeObjetCodeType,
      DemandeMiseEnServiceCorrectifMotifCodeType,
      DemandeMediaCodeType,
      DemandeDiverseReseauTypeCodeType,
      DemandeDiverseQualiteFournitureTypeCodeType,
      DemandeDiverseInformationsTypeCodeType,
      DemandeDiverseComptageTypeCodeType,
      DemandeCommunicationDistributeurTypeCodeType,
      DemandeChangementFrnCorrectifMotifCodeType,
      CreneauHoraireCodeType,
      CourrierModeleCodeType,
      CoupureMotifCodeType,
      CoupureLocalisationCodeType,
      ConsuelMotifNonExigibiliteCodeType,
      ConsommationTypeCodeType,
      CompteurTensionNominaleCodeType,
      CompteurModeleSousTypeCodeType,
      CompteurIntensiteNominaleCodeType,
      CommuneFranceCodeInseeType,
      ClientPrioritaireTypeCodeType,
      ClientFinalCategorieCodeType,
      ClasseTemporelleCodeType,
      CircuitTempoCodeType,
      Chaine255Type,
      Chaine15Type,
      CentreGeographiqueCodeType,
      CalendrierCodeType,
      BooleenType,
      ApplicationCodeType,
      AlimentationSecoursModeBasculeCodeType,
      AlimentationModeApresCompteurCodeType,
      AlimentationEtatCodeType,
      AffaireStatutCodeType,
      AffaireNatureModificationCodeType,
      AffaireModificationMotifRefusCodeType,
      AffaireEtatCodeType,
      AffaireClotureMotifCodeType,
      AffaireAnnulationMotifRefusCodeType,
      AffaireAnnulationMotifCodeType,
      AdresseAfnorLigneType,
      ActiviteSecteurCodeType,
      ActiviteCodeNafType,
      AcheminementTarifCodeType,
      AcceptabiliteResultatCodeType )
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data AcceptabiliteResultatType = AcceptabiliteResultatType
        { acceptabiliteResultatType_code :: Ds.AcceptabiliteResultatCodeType
        , acceptabiliteResultatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AcceptabiliteResultatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AcceptabiliteResultatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AcceptabiliteResultatType{} =
        toXMLElement s [ toXMLAttribute "code" $ acceptabiliteResultatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ acceptabiliteResultatType_libelle x
            ]
 
data AcheminementTarifType = AcheminementTarifType
        { acheminementTarifType_code :: Ds.AcheminementTarifCodeType
        , acheminementTarifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AcheminementTarifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AcheminementTarifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AcheminementTarifType{} =
        toXMLElement s [ toXMLAttribute "code" $ acheminementTarifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ acheminementTarifType_libelle x
            ]
 
data ActiviteSecteurType = ActiviteSecteurType
        { activiteSecteurType_code :: Ds.ActiviteSecteurCodeType
        , activiteSecteurType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ActiviteSecteurType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ActiviteSecteurType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ActiviteSecteurType{} =
        toXMLElement s [ toXMLAttribute "code" $ activiteSecteurType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ activiteSecteurType_libelle x
            ]
 
data ActiviteType = ActiviteType
        { activiteType_code :: Ds.ActiviteCodeNafType
        , activiteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ActiviteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ActiviteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ActiviteType{} =
        toXMLElement s [ toXMLAttribute "code" $ activiteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ activiteType_libelle x
            ]
 
data AdresseAfnorType = AdresseAfnorType
        { adresseAfnorType_ligne1 :: Maybe Ds.AdresseAfnorLigneType
        , adresseAfnorType_ligne2 :: Maybe Ds.AdresseAfnorLigneType
        , adresseAfnorType_ligne3 :: Maybe Ds.AdresseAfnorLigneType
        , adresseAfnorType_ligne4 :: Maybe Ds.AdresseAfnorLigneType
        , adresseAfnorType_ligne5 :: Maybe Ds.AdresseAfnorLigneType
        , adresseAfnorType_ligne6 :: Ds.AdresseAfnorLigneType
        , adresseAfnorType_ligne7 :: Maybe Ds.AdresseAfnorLigneType
        }
        deriving (Eq,Show)
instance SchemaType AdresseAfnorType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AdresseAfnorType
            `apply` optional (parseSchemaType "ligne1")
            `apply` optional (parseSchemaType "ligne2")
            `apply` optional (parseSchemaType "ligne3")
            `apply` optional (parseSchemaType "ligne4")
            `apply` optional (parseSchemaType "ligne5")
            `apply` parseSchemaType "ligne6"
            `apply` optional (parseSchemaType "ligne7")
    schemaTypeToXML s x@AdresseAfnorType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "ligne1") $ adresseAfnorType_ligne1 x
            , maybe [] (schemaTypeToXML "ligne2") $ adresseAfnorType_ligne2 x
            , maybe [] (schemaTypeToXML "ligne3") $ adresseAfnorType_ligne3 x
            , maybe [] (schemaTypeToXML "ligne4") $ adresseAfnorType_ligne4 x
            , maybe [] (schemaTypeToXML "ligne5") $ adresseAfnorType_ligne5 x
            , schemaTypeToXML "ligne6" $ adresseAfnorType_ligne6 x
            , maybe [] (schemaTypeToXML "ligne7") $ adresseAfnorType_ligne7 x
            ]
 
data AffaireAnnulationMotifRefusType = AffaireAnnulationMotifRefusType
        { affaireAnnulationMotifRefusType_code :: Ds.AffaireAnnulationMotifRefusCodeType
        , affaireAnnulationMotifRefusType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AffaireAnnulationMotifRefusType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AffaireAnnulationMotifRefusType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AffaireAnnulationMotifRefusType{} =
        toXMLElement s [ toXMLAttribute "code" $ affaireAnnulationMotifRefusType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ affaireAnnulationMotifRefusType_libelle x
            ]
 
data AffaireAnnulationMotifType = AffaireAnnulationMotifType
        { affaireAnnulationMotifType_code :: Ds.AffaireAnnulationMotifCodeType
        , affaireAnnulationMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AffaireAnnulationMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AffaireAnnulationMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AffaireAnnulationMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ affaireAnnulationMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ affaireAnnulationMotifType_libelle x
            ]
 
data AffaireClotureMotifType = AffaireClotureMotifType
        { affaireClotureMotifType_code :: Ds.AffaireClotureMotifCodeType
        , affaireClotureMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AffaireClotureMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AffaireClotureMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AffaireClotureMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ affaireClotureMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ affaireClotureMotifType_libelle x
            ]
 
data AffaireEtatType = AffaireEtatType
        { affaireEtatType_code :: Ds.AffaireEtatCodeType
        , affaireEtatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AffaireEtatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AffaireEtatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AffaireEtatType{} =
        toXMLElement s [ toXMLAttribute "code" $ affaireEtatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ affaireEtatType_libelle x
            ]
 
data AffaireModificationMotifRefusType = AffaireModificationMotifRefusType
        { affaireModificationMotifRefusType_libelle :: Maybe Ds.Chaine255Type
        , affaireModificationMotifRefusType_code :: Ds.AffaireModificationMotifRefusCodeType
        }
        deriving (Eq,Show)
instance SchemaType AffaireModificationMotifRefusType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AffaireModificationMotifRefusType
            `apply` optional (parseSchemaType "libelle")
            `apply` parseSchemaType "code"
    schemaTypeToXML s x@AffaireModificationMotifRefusType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "libelle") $ affaireModificationMotifRefusType_libelle x
            , schemaTypeToXML "code" $ affaireModificationMotifRefusType_code x
            ]
 
data AffaireNatureModificationType = AffaireNatureModificationType
        { affaireNatureModificationType_libelle :: Maybe Ds.Chaine255Type
        , affaireNatureModificationType_code :: Ds.AffaireNatureModificationCodeType
        }
        deriving (Eq,Show)
instance SchemaType AffaireNatureModificationType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return AffaireNatureModificationType
            `apply` optional (parseSchemaType "libelle")
            `apply` parseSchemaType "code"
    schemaTypeToXML s x@AffaireNatureModificationType{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "libelle") $ affaireNatureModificationType_libelle x
            , schemaTypeToXML "code" $ affaireNatureModificationType_code x
            ]
 
data AffaireStatutType = AffaireStatutType
        { affaireStatutType_code :: Ds.AffaireStatutCodeType
        , affaireStatutType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AffaireStatutType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AffaireStatutType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AffaireStatutType{} =
        toXMLElement s [ toXMLAttribute "code" $ affaireStatutType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ affaireStatutType_libelle x
            ]
 
data AlimentationEtatType = AlimentationEtatType
        { alimentationEtatType_code :: Ds.AlimentationEtatCodeType
        , alimentationEtatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AlimentationEtatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AlimentationEtatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AlimentationEtatType{} =
        toXMLElement s [ toXMLAttribute "code" $ alimentationEtatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ alimentationEtatType_libelle x
            ]
 
data AlimentationModeApresCompteurType = AlimentationModeApresCompteurType
        { alimentationModeApresCompteurType_code :: Ds.AlimentationModeApresCompteurCodeType
        , alimentationModeApresCompteurType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AlimentationModeApresCompteurType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AlimentationModeApresCompteurType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AlimentationModeApresCompteurType{} =
        toXMLElement s [ toXMLAttribute "code" $ alimentationModeApresCompteurType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ alimentationModeApresCompteurType_libelle x
            ]
 
data AlimentationSecoursModeBasculeType = AlimentationSecoursModeBasculeType
        { alimentationSecoursModeBasculeType_code :: Ds.AlimentationSecoursModeBasculeCodeType
        , alimentationSecoursModeBasculeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType AlimentationSecoursModeBasculeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (AlimentationSecoursModeBasculeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@AlimentationSecoursModeBasculeType{} =
        toXMLElement s [ toXMLAttribute "code" $ alimentationSecoursModeBasculeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ alimentationSecoursModeBasculeType_libelle x
            ]
 
data ApplicationType = ApplicationType
        { applicationType_code :: Ds.ApplicationCodeType
        , applicationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ApplicationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ApplicationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ApplicationType{} =
        toXMLElement s [ toXMLAttribute "code" $ applicationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ applicationType_libelle x
            ]
 
data BilanContinuiteFournitureCoupureTypeType = BilanContinuiteFournitureCoupureTypeType
        { bilanContinuiteFournitureCoupureTypeType_code :: Ds.OptionBilanContinuiteFournitureCoupureTypeCodeType
        , bilanContinuiteFournitureCoupureTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType BilanContinuiteFournitureCoupureTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (BilanContinuiteFournitureCoupureTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@BilanContinuiteFournitureCoupureTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ bilanContinuiteFournitureCoupureTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ bilanContinuiteFournitureCoupureTypeType_libelle x
            ]
 
data BilanContinuiteFournitureTypeType = BilanContinuiteFournitureTypeType
        { bilanContinuiteFournitureTypeType_code :: Ds.OptionBilanContinuiteFournitureTypeCodeType
        , bilanContinuiteFournitureTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType BilanContinuiteFournitureTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (BilanContinuiteFournitureTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@BilanContinuiteFournitureTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ bilanContinuiteFournitureTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ bilanContinuiteFournitureTypeType_libelle x
            ]
 
data CalendrierType = CalendrierType
        { calendrierType_code :: Ds.CalendrierCodeType
        , calendrierType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CalendrierType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CalendrierType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CalendrierType{} =
        toXMLElement s [ toXMLAttribute "code" $ calendrierType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ calendrierType_libelle x
            ]
 
data CentreGeographiqueType = CentreGeographiqueType
        { centreGeographiqueType_code :: Ds.CentreGeographiqueCodeType
        , centreGeographiqueType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CentreGeographiqueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CentreGeographiqueType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CentreGeographiqueType{} =
        toXMLElement s [ toXMLAttribute "code" $ centreGeographiqueType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ centreGeographiqueType_libelle x
            ]
 
data CircuitTempoType = CircuitTempoType
        { circuitTempoType_code :: Ds.CircuitTempoCodeType
        , circuitTempoType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CircuitTempoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CircuitTempoType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CircuitTempoType{} =
        toXMLElement s [ toXMLAttribute "code" $ circuitTempoType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ circuitTempoType_libelle x
            ]
 
data ClasseTemporelleType = ClasseTemporelleType
        { classeTemporelleType_code :: Ds.ClasseTemporelleCodeType
        , classeTemporelleType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ClasseTemporelleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ClasseTemporelleType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ClasseTemporelleType{} =
        toXMLElement s [ toXMLAttribute "code" $ classeTemporelleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ classeTemporelleType_libelle x
            ]
 
data ClientFinalCategorieType = ClientFinalCategorieType
        { clientFinalCategorieType_code :: Ds.ClientFinalCategorieCodeType
        , clientFinalCategorieType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ClientFinalCategorieType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ClientFinalCategorieType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ClientFinalCategorieType{} =
        toXMLElement s [ toXMLAttribute "code" $ clientFinalCategorieType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ clientFinalCategorieType_libelle x
            ]
 
data ClientPrioritaireTypeType = ClientPrioritaireTypeType
        { clientPrioritaireTypeType_code :: Ds.ClientPrioritaireTypeCodeType
        , clientPrioritaireTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ClientPrioritaireTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ClientPrioritaireTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ClientPrioritaireTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ clientPrioritaireTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ clientPrioritaireTypeType_libelle x
            ]
 
data CommuneFranceType = CommuneFranceType
        { communeFranceType_code :: Ds.CommuneFranceCodeInseeType
        , communeFranceType_libelle :: Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CommuneFranceType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CommuneFranceType a0)
            `apply` parseSchemaType "libelle"
    schemaTypeToXML s x@CommuneFranceType{} =
        toXMLElement s [ toXMLAttribute "code" $ communeFranceType_code x
                       ]
            [ schemaTypeToXML "libelle" $ communeFranceType_libelle x
            ]
 
data CompteurIntensiteNominaleType = CompteurIntensiteNominaleType
        { compteurIntensiteNominaleType_code :: Ds.CompteurIntensiteNominaleCodeType
        , compteurIntensiteNominaleType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CompteurIntensiteNominaleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CompteurIntensiteNominaleType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CompteurIntensiteNominaleType{} =
        toXMLElement s [ toXMLAttribute "code" $ compteurIntensiteNominaleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ compteurIntensiteNominaleType_libelle x
            ]
 
data CompteurSousTypeType = CompteurSousTypeType
        { compteurSousTypeType_code :: Ds.CompteurModeleSousTypeCodeType
        , compteurSousTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CompteurSousTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CompteurSousTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CompteurSousTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ compteurSousTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ compteurSousTypeType_libelle x
            ]
 
data CompteurTensionNominaleType = CompteurTensionNominaleType
        { compteurTensionNominaleType_code :: Ds.CompteurTensionNominaleCodeType
        , compteurTensionNominaleType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CompteurTensionNominaleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CompteurTensionNominaleType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CompteurTensionNominaleType{} =
        toXMLElement s [ toXMLAttribute "code" $ compteurTensionNominaleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ compteurTensionNominaleType_libelle x
            ]
 
data ConsommationTypeType = ConsommationTypeType
        { consommationTypeType_code :: Ds.ConsommationTypeCodeType
        , consommationTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ConsommationTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ConsommationTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ConsommationTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ consommationTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ consommationTypeType_libelle x
            ]
 
data ConsuelMotifNonExigibiliteType = ConsuelMotifNonExigibiliteType
        { consuelMotifNonExigibiliteType_code :: Ds.ConsuelMotifNonExigibiliteCodeType
        , consuelMotifNonExigibiliteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ConsuelMotifNonExigibiliteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ConsuelMotifNonExigibiliteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ConsuelMotifNonExigibiliteType{} =
        toXMLElement s [ toXMLAttribute "code" $ consuelMotifNonExigibiliteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ consuelMotifNonExigibiliteType_libelle x
            ]
 
data CoordonnesGpsType = CoordonnesGpsType
        { coordonnesGpsType_latitude :: Ds.NbDecimalType
        , coordonnesGpsType_longitude :: Ds.NbDecimalType
        }
        deriving (Eq,Show)
instance SchemaType CoordonnesGpsType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return CoordonnesGpsType
            `apply` parseSchemaType "latitude"
            `apply` parseSchemaType "longitude"
    schemaTypeToXML s x@CoordonnesGpsType{} =
        toXMLElement s []
            [ schemaTypeToXML "latitude" $ coordonnesGpsType_latitude x
            , schemaTypeToXML "longitude" $ coordonnesGpsType_longitude x
            ]
 
data CoupureLocalisationType = CoupureLocalisationType
        { coupureLocalisationType_code :: Ds.CoupureLocalisationCodeType
        , coupureLocalisationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CoupureLocalisationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CoupureLocalisationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CoupureLocalisationType{} =
        toXMLElement s [ toXMLAttribute "code" $ coupureLocalisationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ coupureLocalisationType_libelle x
            ]
 
data CoupureMotifType = CoupureMotifType
        { coupureMotifType_code :: Ds.CoupureMotifCodeType
        , coupureMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CoupureMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CoupureMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CoupureMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ coupureMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ coupureMotifType_libelle x
            ]
 
data CourrierModeleType = CourrierModeleType
        { courrierModeleType_code :: Ds.CourrierModeleCodeType
        , courrierModeleType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CourrierModeleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CourrierModeleType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CourrierModeleType{} =
        toXMLElement s [ toXMLAttribute "code" $ courrierModeleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ courrierModeleType_libelle x
            ]
 
data CreneauHoraireType = CreneauHoraireType
        { creneauHoraireType_code :: Ds.CreneauHoraireCodeType
        , creneauHoraireType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType CreneauHoraireType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (CreneauHoraireType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@CreneauHoraireType{} =
        toXMLElement s [ toXMLAttribute "code" $ creneauHoraireType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ creneauHoraireType_libelle x
            ]
 
data DemandeChangementFrnCorrectifMotifType = DemandeChangementFrnCorrectifMotifType
        { demandeChangementFrnCorrectifMotifType_code :: Ds.DemandeChangementFrnCorrectifMotifCodeType
        , demandeChangementFrnCorrectifMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeChangementFrnCorrectifMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeChangementFrnCorrectifMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeChangementFrnCorrectifMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeChangementFrnCorrectifMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeChangementFrnCorrectifMotifType_libelle x
            ]
 
data DemandeCommunicationDistributeurTypeType = DemandeCommunicationDistributeurTypeType
        { demandeCommunicationDistributeurTypeType_code :: Ds.DemandeCommunicationDistributeurTypeCodeType
        , demandeCommunicationDistributeurTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeCommunicationDistributeurTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeCommunicationDistributeurTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeCommunicationDistributeurTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeCommunicationDistributeurTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeCommunicationDistributeurTypeType_libelle x
            ]
 
data DemandeDiverseComptageTypeType = DemandeDiverseComptageTypeType
        { demandeDiverseComptageTypeType_code :: Ds.DemandeDiverseComptageTypeCodeType
        , demandeDiverseComptageTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeDiverseComptageTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeDiverseComptageTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeDiverseComptageTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeDiverseComptageTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeDiverseComptageTypeType_libelle x
            ]
 
data DemandeDiverseInformationsTypeType = DemandeDiverseInformationsTypeType
        { demandeDiverseInformationsTypeType_code :: Ds.DemandeDiverseInformationsTypeCodeType
        , demandeDiverseInformationsTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeDiverseInformationsTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeDiverseInformationsTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeDiverseInformationsTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeDiverseInformationsTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeDiverseInformationsTypeType_libelle x
            ]
 
data DemandeDiverseQualiteFournitureTypeType = DemandeDiverseQualiteFournitureTypeType
        { demandeDiverseQualiteFournitureTypeType_code :: Ds.DemandeDiverseQualiteFournitureTypeCodeType
        , demandeDiverseQualiteFournitureTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeDiverseQualiteFournitureTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeDiverseQualiteFournitureTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeDiverseQualiteFournitureTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeDiverseQualiteFournitureTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeDiverseQualiteFournitureTypeType_libelle x
            ]
 
data DemandeDiverseReseauTypeType = DemandeDiverseReseauTypeType
        { demandeDiverseReseauTypeType_code :: Ds.DemandeDiverseReseauTypeCodeType
        , demandeDiverseReseauTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeDiverseReseauTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeDiverseReseauTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeDiverseReseauTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeDiverseReseauTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeDiverseReseauTypeType_libelle x
            ]
 
data DemandeMediaType = DemandeMediaType
        { demandeMediaType_code :: Ds.DemandeMediaCodeType
        , demandeMediaType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeMediaType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeMediaType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeMediaType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeMediaType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeMediaType_libelle x
            ]
 
data DemandeMiseEnServiceCorrectiveMotifType = DemandeMiseEnServiceCorrectiveMotifType
        { demandeMiseEnServiceCorrectiveMotifType_code :: Ds.DemandeMiseEnServiceCorrectifMotifCodeType
        , demandeMiseEnServiceCorrectiveMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeMiseEnServiceCorrectiveMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeMiseEnServiceCorrectiveMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeMiseEnServiceCorrectiveMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeMiseEnServiceCorrectiveMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeMiseEnServiceCorrectiveMotifType_libelle x
            ]
 
data DemandeObjetType = DemandeObjetType
        { demandeObjetType_code :: Ds.DemandeObjetCodeType
        , demandeObjetType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeObjetType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeObjetType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeObjetType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeObjetType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeObjetType_libelle x
            ]
 
data DemandeTechniqueTypeType = DemandeTechniqueTypeType
        { demandeTechniqueTypeType_code :: Ds.DemandeTechniqueTypeCodeType
        , demandeTechniqueTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DemandeTechniqueTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DemandeTechniqueTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DemandeTechniqueTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ demandeTechniqueTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ demandeTechniqueTypeType_libelle x
            ]
 
data DisjoncteurCalibreType = DisjoncteurCalibreType
        { disjoncteurCalibreType_code :: Ds.DisjoncteurCalibreCodeType
        , disjoncteurCalibreType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DisjoncteurCalibreType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DisjoncteurCalibreType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DisjoncteurCalibreType{} =
        toXMLElement s [ toXMLAttribute "code" $ disjoncteurCalibreType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ disjoncteurCalibreType_libelle x
            ]
 
data DisjoncteurNatureType = DisjoncteurNatureType
        { disjoncteurNatureType_code :: Ds.DisjoncteurNatureCodeType
        , disjoncteurNatureType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DisjoncteurNatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DisjoncteurNatureType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DisjoncteurNatureType{} =
        toXMLElement s [ toXMLAttribute "code" $ disjoncteurNatureType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ disjoncteurNatureType_libelle x
            ]
 
data DispositifComptageParticulariteType = DispositifComptageParticulariteType
        { dispositifComptageParticulariteType_code :: Ds.DispositifComptageParticulariteCodeType
        , dispositifComptageParticulariteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DispositifComptageParticulariteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DispositifComptageParticulariteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DispositifComptageParticulariteType{} =
        toXMLElement s [ toXMLAttribute "code" $ dispositifComptageParticulariteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ dispositifComptageParticulariteType_libelle x
            ]
 
data DispositifParticulierLimitationPerturbationsType = DispositifParticulierLimitationPerturbationsType
        { dispositifParticulierLimitationPerturbationsType_code :: Ds.DispositifParticulierLimitationPerturbationsCodeType
        , dispositifParticulierLimitationPerturbationsType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DispositifParticulierLimitationPerturbationsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DispositifParticulierLimitationPerturbationsType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DispositifParticulierLimitationPerturbationsType{} =
        toXMLElement s [ toXMLAttribute "code" $ dispositifParticulierLimitationPerturbationsType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ dispositifParticulierLimitationPerturbationsType_libelle x
            ]
 
data DomaineTensionType = DomaineTensionType
        { domaineTensionType_code :: Ds.DomaineTensionCodeType
        , domaineTensionType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DomaineTensionType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DomaineTensionType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DomaineTensionType{} =
        toXMLElement s [ toXMLAttribute "code" $ domaineTensionType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ domaineTensionType_libelle x
            ]
 
data DureeType = DureeType
        { dureeType_valeur :: Ds.NbDecimalType
        , dureeType_unite :: Ds.DureeUniteSymboleType
        }
        deriving (Eq,Show)
instance SchemaType DureeType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return DureeType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@DureeType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ dureeType_valeur x
            , schemaTypeToXML "unite" $ dureeType_unite x
            ]
 
data DysfonctionnementNatureType = DysfonctionnementNatureType
        { dysfonctionnementNatureType_code :: Ds.DysfonctionnementNatureCodeType
        , dysfonctionnementNatureType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType DysfonctionnementNatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (DysfonctionnementNatureType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@DysfonctionnementNatureType{} =
        toXMLElement s [ toXMLAttribute "code" $ dysfonctionnementNatureType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ dysfonctionnementNatureType_libelle x
            ]
 
data EquipementElectriqueLocalisationType = EquipementElectriqueLocalisationType
        { equipementElectriqueLocalisationType_code :: Ds.EquipementElectriqueLocalisationCodeType
        , equipementElectriqueLocalisationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType EquipementElectriqueLocalisationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (EquipementElectriqueLocalisationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@EquipementElectriqueLocalisationType{} =
        toXMLElement s [ toXMLAttribute "code" $ equipementElectriqueLocalisationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ equipementElectriqueLocalisationType_libelle x
            ]
 
data EquipementElectriqueRegimeProprieteType = EquipementElectriqueRegimeProprieteType
        { equipementElectriqueRegimeProprieteType_code :: Ds.EquipementElectriqueRegimeProprieteCodeType
        , equipementElectriqueRegimeProprieteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType EquipementElectriqueRegimeProprieteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (EquipementElectriqueRegimeProprieteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@EquipementElectriqueRegimeProprieteType{} =
        toXMLElement s [ toXMLAttribute "code" $ equipementElectriqueRegimeProprieteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ equipementElectriqueRegimeProprieteType_libelle x
            ]
 
data FaisabiliteMotifImpossibiliteType = FaisabiliteMotifImpossibiliteType
        { faisabiliteMotifImpossibiliteType_code :: Maybe Ds.FaisabiliteMotifImpossibiliteCodeType
        , faisabiliteMotifImpossibiliteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FaisabiliteMotifImpossibiliteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "code" e pos
        commit $ interior e $ return (FaisabiliteMotifImpossibiliteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FaisabiliteMotifImpossibiliteType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "code") $ faisabiliteMotifImpossibiliteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ faisabiliteMotifImpossibiliteType_libelle x
            ]
 
data FaisabiliteReserveMotifType = FaisabiliteReserveMotifType
        { faisabiliteReserveMotifType_code :: Ds.FaisabiliteReserveMotifCodeType
        , faisabiliteReserveMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FaisabiliteReserveMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FaisabiliteReserveMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FaisabiliteReserveMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ faisabiliteReserveMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ faisabiliteReserveMotifType_libelle x
            ]
 
data FaisabiliteResultatType = FaisabiliteResultatType
        { faisabiliteResultatType_code :: Ds.BooleenType
        , faisabiliteResultatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FaisabiliteResultatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FaisabiliteResultatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FaisabiliteResultatType{} =
        toXMLElement s [ toXMLAttribute "code" $ faisabiliteResultatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ faisabiliteResultatType_libelle x
            ]
 
data FinaliteType = FinaliteType
        { finaliteType_code :: Ds.FinaliteCodeType
        , finaliteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FinaliteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FinaliteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FinaliteType{} =
        toXMLElement s [ toXMLAttribute "code" $ finaliteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ finaliteType_libelle x
            ]
 
data FraisType = FraisType
        { fraisType_code :: Ds.FraisCodeType
        , fraisType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FraisType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FraisType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FraisType{} =
        toXMLElement s [ toXMLAttribute "code" $ fraisType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ fraisType_libelle x
            ]
 
data FraudeNatureType = FraudeNatureType
        { fraudeNatureType_code :: Ds.FraudeNatureCodeType
        , fraudeNatureType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FraudeNatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FraudeNatureType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FraudeNatureType{} =
        toXMLElement s [ toXMLAttribute "code" $ fraudeNatureType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ fraudeNatureType_libelle x
            ]
 
data FraudeOuDysfonctionnementMethodeCalculType = FraudeOuDysfonctionnementMethodeCalculType
        { fraudeOuDysfonctionnementMethodeCalculType_code :: Ds.FraudeOuDysfonctionnementMethodeCalculCodeType
        , fraudeOuDysfonctionnementMethodeCalculType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FraudeOuDysfonctionnementMethodeCalculType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FraudeOuDysfonctionnementMethodeCalculType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FraudeOuDysfonctionnementMethodeCalculType{} =
        toXMLElement s [ toXMLAttribute "code" $ fraudeOuDysfonctionnementMethodeCalculType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ fraudeOuDysfonctionnementMethodeCalculType_libelle x
            ]
 
data FraudeOuDysfonctionnementnMethodeFacturationType = FraudeOuDysfonctionnementnMethodeFacturationType
        { fraudeOuDysfonctionnementnMethodeFacturationType_code :: Ds.FraudeOuDysfonctionnementMethodeFacturationCodeType
        , fraudeOuDysfonctionnementnMethodeFacturationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType FraudeOuDysfonctionnementnMethodeFacturationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (FraudeOuDysfonctionnementnMethodeFacturationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@FraudeOuDysfonctionnementnMethodeFacturationType{} =
        toXMLElement s [ toXMLAttribute "code" $ fraudeOuDysfonctionnementnMethodeFacturationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ fraudeOuDysfonctionnementnMethodeFacturationType_libelle x
            ]
 
data GeneriqueType = GeneriqueType
        { generiqueType_code :: Ds.Chaine15Type
        , generiqueType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType GeneriqueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (GeneriqueType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@GeneriqueType{} =
        toXMLElement s [ toXMLAttribute "code" $ generiqueType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ generiqueType_libelle x
            ]
 
data GrandeurPhysiqueType = GrandeurPhysiqueType
        { grandeurPhysiqueType_code :: Ds.GrandeurPhysiqueCodeType
        , grandeurPhysiqueType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType GrandeurPhysiqueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (GrandeurPhysiqueType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@GrandeurPhysiqueType{} =
        toXMLElement s [ toXMLAttribute "code" $ grandeurPhysiqueType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ grandeurPhysiqueType_libelle x
            ]
 
data GroupePeriodeMobileType = GroupePeriodeMobileType
        { groupePeriodeMobileType_code :: Ds.GroupePeriodeMobileCodeType
        , groupePeriodeMobileType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType GroupePeriodeMobileType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (GroupePeriodeMobileType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@GroupePeriodeMobileType{} =
        toXMLElement s [ toXMLAttribute "code" $ groupePeriodeMobileType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ groupePeriodeMobileType_libelle x
            ]
 
data IntensiteType = IntensiteType
        { intensiteType_valeur :: Ds.NbDecimalType
        , intensiteType_unite :: Ds.IntensiteUniteSymboleType
        }
        deriving (Eq,Show)
instance SchemaType IntensiteType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return IntensiteType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@IntensiteType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ intensiteType_valeur x
            , schemaTypeToXML "unite" $ intensiteType_unite x
            ]
 
data IntervenantTypeType = IntervenantTypeType
        { intervenantTypeType_code :: Ds.IntervenantTypeCodeType
        , intervenantTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType IntervenantTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (IntervenantTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@IntervenantTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ intervenantTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ intervenantTypeType_libelle x
            ]
 
data InterventionDemandeMotifRefusType = InterventionDemandeMotifRefusType
        { interventionDemandeMotifRefusType_code :: Ds.InterventionDemandeMotifRefusCodeType
        , interventionDemandeMotifRefusType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionDemandeMotifRefusType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionDemandeMotifRefusType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionDemandeMotifRefusType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionDemandeMotifRefusType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionDemandeMotifRefusType_libelle x
            ]
 
data InterventionDeplanificationMotifType = InterventionDeplanificationMotifType
        { interventionDeplanificationMotifType_code :: Ds.InterventionDeplanificationMotifCodeType
        , interventionDeplanificationMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionDeplanificationMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionDeplanificationMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionDeplanificationMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionDeplanificationMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionDeplanificationMotifType_libelle x
            ]
 
data InterventionEtatType = InterventionEtatType
        { interventionEtatType_code :: Ds.InterventionEtatCodeType
        , interventionEtatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionEtatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionEtatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionEtatType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionEtatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionEtatType_libelle x
            ]
 
data InterventionNonRealisationMotifType = InterventionNonRealisationMotifType
        { interventionNonRealisationMotifType_code :: Ds.InterventionNonRealisationMotifCodeType
        , interventionNonRealisationMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionNonRealisationMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionNonRealisationMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionNonRealisationMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionNonRealisationMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionNonRealisationMotifType_libelle x
            ]
 
data InterventionPeriodeTypeType = InterventionPeriodeTypeType
        { interventionPeriodeTypeType_code :: Ds.InterventionPeriodeTypeCodeType
        , interventionPeriodeTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionPeriodeTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionPeriodeTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionPeriodeTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionPeriodeTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionPeriodeTypeType_libelle x
            ]
 
data InterventionPlanificationHorsDelaiMotifType = InterventionPlanificationHorsDelaiMotifType
        { interventionPlanificationHorsDelaiMotifType_code :: Ds.InterventionPlanificationHorsDelaiMotifCodeType
        , interventionPlanificationHorsDelaiMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionPlanificationHorsDelaiMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionPlanificationHorsDelaiMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionPlanificationHorsDelaiMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionPlanificationHorsDelaiMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionPlanificationHorsDelaiMotifType_libelle x
            ]
 
data InterventionRealisationEtatType = InterventionRealisationEtatType
        { interventionRealisationEtatType_code :: Ds.InterventionRealisationEtatCodeType
        , interventionRealisationEtatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionRealisationEtatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionRealisationEtatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionRealisationEtatType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionRealisationEtatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionRealisationEtatType_libelle x
            ]
 
data InterventionReplanificationMotifType = InterventionReplanificationMotifType
        { interventionReplanificationMotifType_code :: Ds.InterventionReplanificationMotifCodeType
        , interventionReplanificationMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType InterventionReplanificationMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (InterventionReplanificationMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@InterventionReplanificationMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ interventionReplanificationMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ interventionReplanificationMotifType_libelle x
            ]
 
data LigneFraisType = LigneFraisType
        { ligneFraisType_frais :: FraisType
        , ligneFraisType_quantite :: Ds.NbEntierType
        }
        deriving (Eq,Show)
instance SchemaType LigneFraisType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return LigneFraisType
            `apply` parseSchemaType "frais"
            `apply` parseSchemaType "quantite"
    schemaTypeToXML s x@LigneFraisType{} =
        toXMLElement s []
            [ schemaTypeToXML "frais" $ ligneFraisType_frais x
            , schemaTypeToXML "quantite" $ ligneFraisType_quantite x
            ]
 
newtype LignesFraisType = LignesFraisType
        { lignesFraisType_ligneFrais :: [LigneFraisType]
        }
        deriving (Eq,Show)
instance SchemaType LignesFraisType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return LignesFraisType
            `apply` between (Occurs Nothing (Just 200))
                            (parseSchemaType "ligneFrais")
    schemaTypeToXML s x@LignesFraisType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "ligneFrais") $ lignesFraisType_ligneFrais x
            ]
 
data LimiteurTypeType = LimiteurTypeType
        { limiteurTypeType_code :: Ds.LimiteurTypeCodeType
        , limiteurTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType LimiteurTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (LimiteurTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@LimiteurTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ limiteurTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ limiteurTypeType_libelle x
            ]
 
data LongueurType = LongueurType
        { longueurType_valeur :: Ds.NbDecimalType
        , longueurType_unite :: Ds.LongueurUniteSymboleType
        }
        deriving (Eq,Show)
instance SchemaType LongueurType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return LongueurType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@LongueurType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ longueurType_valeur x
            , schemaTypeToXML "unite" $ longueurType_unite x
            ]
 
data MesureDeclencheurType = MesureDeclencheurType
        { mesureDeclencheurType_code :: Ds.MesureDeclencheurCodeType
        , mesureDeclencheurType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType MesureDeclencheurType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (MesureDeclencheurType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@MesureDeclencheurType{} =
        toXMLElement s [ toXMLAttribute "code" $ mesureDeclencheurType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ mesureDeclencheurType_libelle x
            ]
 
data MesureNatureType = MesureNatureType
        { mesureNatureType_code :: Ds.MesureNatureCodeType
        , mesureNatureType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType MesureNatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (MesureNatureType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@MesureNatureType{} =
        toXMLElement s [ toXMLAttribute "code" $ mesureNatureType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ mesureNatureType_libelle x
            ]
 
data MesureOrigineType = MesureOrigineType
        { mesureOrigineType_code :: Ds.MesureOrigineCodeType
        , mesureOrigineType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType MesureOrigineType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (MesureOrigineType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@MesureOrigineType{} =
        toXMLElement s [ toXMLAttribute "code" $ mesureOrigineType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ mesureOrigineType_libelle x
            ]
 
data MesureStatutType = MesureStatutType
        { mesureStatutType_code :: Ds.MesureStatutCodeType
        , mesureStatutType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType MesureStatutType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (MesureStatutType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@MesureStatutType{} =
        toXMLElement s [ toXMLAttribute "code" $ mesureStatutType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ mesureStatutType_libelle x
            ]
 
data MesureTypeType = MesureTypeType
        { mesureTypeType_code :: Ds.MesureTypeCodeType
        , mesureTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType MesureTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (MesureTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@MesureTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ mesureTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ mesureTypeType_libelle x
            ]
 
data MontantType = MontantType
        { montantType_valeur :: Ds.NbDecimalType
        , montantType_unite :: Ds.DeviseAbreviationType
        }
        deriving (Eq,Show)
instance SchemaType MontantType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return MontantType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@MontantType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ montantType_valeur x
            , schemaTypeToXML "unite" $ montantType_unite x
            ]
 
data NonPriseEnCompteAutoreleveMotifType = NonPriseEnCompteAutoreleveMotifType
        { nonPriseEnCompteAutoreleveMotifType_code :: Ds.NonPriseEnCompteAutoreleveMotifCodeType
        , nonPriseEnCompteAutoreleveMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType NonPriseEnCompteAutoreleveMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (NonPriseEnCompteAutoreleveMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@NonPriseEnCompteAutoreleveMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ nonPriseEnCompteAutoreleveMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ nonPriseEnCompteAutoreleveMotifType_libelle x
            ]
 
data OffreTypeType = OffreTypeType
        { offreTypeType_code :: Ds.OffreTypeCodeType
        , offreTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType OffreTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (OffreTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@OffreTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ offreTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ offreTypeType_libelle x
            ]
 
data OperationType = OperationType
        { operationType_code :: Ds.OperationCodeType
        , operationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType OperationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (OperationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@OperationType{} =
        toXMLElement s [ toXMLAttribute "code" $ operationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ operationType_libelle x
            ]
 
data PeriodiciteType = PeriodiciteType
        { periodiciteType_code :: Ds.PeriodiciteCodeType
        , periodiciteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PeriodiciteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PeriodiciteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PeriodiciteType{} =
        toXMLElement s [ toXMLAttribute "code" $ periodiciteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ periodiciteType_libelle x
            ]
 
data PlageHeuresCreusesType = PlageHeuresCreusesType
        { plageHeuresCreusesType_code :: Ds.PlageHeuresCreusesCodeType
        , plageHeuresCreusesType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PlageHeuresCreusesType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PlageHeuresCreusesType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PlageHeuresCreusesType{} =
        toXMLElement s [ toXMLAttribute "code" $ plageHeuresCreusesType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ plageHeuresCreusesType_libelle x
            ]
 
data PointAppartenanceRegroupementHebergeurDecomptantType = PointAppartenanceRegroupementHebergeurDecomptantType
        { pointAppartenanceRegroupementHebergeurDecomptantType_code :: Ds.PointAppartenanceRegroupementHebergeurDecomptantCodeType
        , pointAppartenanceRegroupementHebergeurDecomptantType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PointAppartenanceRegroupementHebergeurDecomptantType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PointAppartenanceRegroupementHebergeurDecomptantType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PointAppartenanceRegroupementHebergeurDecomptantType{} =
        toXMLElement s [ toXMLAttribute "code" $ pointAppartenanceRegroupementHebergeurDecomptantType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ pointAppartenanceRegroupementHebergeurDecomptantType_libelle x
            ]
 
data PointAppartenanceRegroupementTurpeType = PointAppartenanceRegroupementTurpeType
        { pointAppartenanceRegroupementTurpeType_code :: Ds.PointAppartenanceRegroupementTurpeCodeType
        , pointAppartenanceRegroupementTurpeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PointAppartenanceRegroupementTurpeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PointAppartenanceRegroupementTurpeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PointAppartenanceRegroupementTurpeType{} =
        toXMLElement s [ toXMLAttribute "code" $ pointAppartenanceRegroupementTurpeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ pointAppartenanceRegroupementTurpeType_libelle x
            ]
 
data PointEtatContractuelType = PointEtatContractuelType
        { pointEtatContractuelType_code :: Ds.PointEtatContractuelCodeType
        , pointEtatContractuelType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PointEtatContractuelType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PointEtatContractuelType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PointEtatContractuelType{} =
        toXMLElement s [ toXMLAttribute "code" $ pointEtatContractuelType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ pointEtatContractuelType_libelle x
            ]
 
data PointSegmentClienteleType = PointSegmentClienteleType
        { pointSegmentClienteleType_code :: Ds.PointSegmentClienteleCodeType
        , pointSegmentClienteleType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PointSegmentClienteleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PointSegmentClienteleType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PointSegmentClienteleType{} =
        toXMLElement s [ toXMLAttribute "code" $ pointSegmentClienteleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ pointSegmentClienteleType_libelle x
            ]
 
data PrejudiceNatureType = PrejudiceNatureType
        { prejudiceNatureType_code :: Ds.PrejudiceNatureCodeType
        , prejudiceNatureType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrejudiceNatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrejudiceNatureType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrejudiceNatureType{} =
        toXMLElement s [ toXMLAttribute "code" $ prejudiceNatureType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prejudiceNatureType_libelle x
            ]
 
data PrestationCasType = PrestationCasType
        { prestationCasType_code :: Ds.PrestationCasCodeType
        , prestationCasType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationCasType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationCasType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationCasType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationCasType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationCasType_libelle x
            ]
 
data PrestationFacturationType = PrestationFacturationType
        { prestationFacturationType_code :: Ds.BooleenType
        , prestationFacturationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationFacturationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationFacturationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationFacturationType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationFacturationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationFacturationType_libelle x
            ]
 
data PrestationFicheType = PrestationFicheType
        { prestationFicheType_code :: Ds.PrestationFicheCodeType
        , prestationFicheType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationFicheType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationFicheType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationFicheType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationFicheType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationFicheType_libelle x
            ]
 
data PrestationMotifNonFacturationType = PrestationMotifNonFacturationType
        { prestationMotifNonFacturationType_code :: Ds.PrestationMotifNonFacturationCodeType
        , prestationMotifNonFacturationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationMotifNonFacturationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationMotifNonFacturationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationMotifNonFacturationType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationMotifNonFacturationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationMotifNonFacturationType_libelle x
            ]
 
data PrestationMotifNonRealisationType = PrestationMotifNonRealisationType
        { prestationMotifNonRealisationType_code :: Ds.PrestationMotifNonRealisationCodeType
        , prestationMotifNonRealisationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationMotifNonRealisationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationMotifNonRealisationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationMotifNonRealisationType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationMotifNonRealisationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationMotifNonRealisationType_libelle x
            ]
 
data PrestationOptionType = PrestationOptionType
        { prestationOptionType_code :: Ds.PrestationOptionCodeType
        , prestationOptionType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationOptionType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationOptionType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationOptionType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationOptionType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationOptionType_libelle x
            ]
 
data PrestationRealisationType = PrestationRealisationType
        { prestationRealisationType_code :: Ds.BooleenType
        , prestationRealisationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType PrestationRealisationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (PrestationRealisationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@PrestationRealisationType{} =
        toXMLElement s [ toXMLAttribute "code" $ prestationRealisationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ prestationRealisationType_libelle x
            ]
 
data ProductionAutonomeCouplageModeType = ProductionAutonomeCouplageModeType
        { productionAutonomeCouplageModeType_code :: Ds.ProductionAutonomeCouplageModeCodeType
        , productionAutonomeCouplageModeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ProductionAutonomeCouplageModeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ProductionAutonomeCouplageModeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ProductionAutonomeCouplageModeType{} =
        toXMLElement s [ toXMLAttribute "code" $ productionAutonomeCouplageModeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ productionAutonomeCouplageModeType_libelle x
            ]
 
data ProgrammeCircuitChauffageTempoType = ProgrammeCircuitChauffageTempoType
        { programmeCircuitChauffageTempoType_code :: Ds.ProgrammeCircuitChauffageTempoCodeType
        , programmeCircuitChauffageTempoType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ProgrammeCircuitChauffageTempoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ProgrammeCircuitChauffageTempoType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ProgrammeCircuitChauffageTempoType{} =
        toXMLElement s [ toXMLAttribute "code" $ programmeCircuitChauffageTempoType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ programmeCircuitChauffageTempoType_libelle x
            ]
 
data ProgrammeCircuitEauTempoType = ProgrammeCircuitEauTempoType
        { programmeCircuitEauTempoType_code :: Ds.ProgrammeCircuitEauTempoCodeType
        , programmeCircuitEauTempoType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ProgrammeCircuitEauTempoType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ProgrammeCircuitEauTempoType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ProgrammeCircuitEauTempoType{} =
        toXMLElement s [ toXMLAttribute "code" $ programmeCircuitEauTempoType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ programmeCircuitEauTempoType_libelle x
            ]
 
data PuissanceType = PuissanceType
        { puissanceType_valeur :: Ds.NbDecimalType
        , puissanceType_unite :: Ds.PuissanceUniteSymboleType
        }
        deriving (Eq,Show)
instance SchemaType PuissanceType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return PuissanceType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@PuissanceType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ puissanceType_valeur x
            , schemaTypeToXML "unite" $ puissanceType_unite x
            ]
 
data RattachementPointRoleType = RattachementPointRoleType
        { rattachementPointRoleType_code :: Ds.RattachementPointRoleCodeType
        , rattachementPointRoleType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RattachementPointRoleType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RattachementPointRoleType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RattachementPointRoleType{} =
        toXMLElement s [ toXMLAttribute "code" $ rattachementPointRoleType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ rattachementPointRoleType_libelle x
            ]
 
data RattachementTypeType = RattachementTypeType
        { rattachementTypeType_code :: Ds.RattachementTypeCodeType
        , rattachementTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RattachementTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RattachementTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RattachementTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ rattachementTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ rattachementTypeType_libelle x
            ]
 
data RecevabiliteMotifRefusType = RecevabiliteMotifRefusType
        { recevabiliteMotifRefusType_code :: Ds.RecevabiliteMotifRefusCodeType
        , recevabiliteMotifRefusType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RecevabiliteMotifRefusType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RecevabiliteMotifRefusType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RecevabiliteMotifRefusType{} =
        toXMLElement s [ toXMLAttribute "code" $ recevabiliteMotifRefusType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ recevabiliteMotifRefusType_libelle x
            ]
 
data RecevabilitePremiereMiseEnServicePourEssaiMotifType = RecevabilitePremiereMiseEnServicePourEssaiMotifType
        { recevabilitePremiereMiseEnServicePourEssaiMotifType_code :: Ds.RecevabilitePremiereMiseEnServicePourEssaiMotifCodeType
        , recevabilitePremiereMiseEnServicePourEssaiMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RecevabilitePremiereMiseEnServicePourEssaiMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RecevabilitePremiereMiseEnServicePourEssaiMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RecevabilitePremiereMiseEnServicePourEssaiMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ recevabilitePremiereMiseEnServicePourEssaiMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ recevabilitePremiereMiseEnServicePourEssaiMotifType_libelle x
            ]
 
data RecevabiliteResultatType = RecevabiliteResultatType
        { recevabiliteResultatType_code :: Ds.RecevabiliteResultatCodeType
        , recevabiliteResultatType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RecevabiliteResultatType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RecevabiliteResultatType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RecevabiliteResultatType{} =
        toXMLElement s [ toXMLAttribute "code" $ recevabiliteResultatType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ recevabiliteResultatType_libelle x
            ]
 
data ReclamationSousTypeType = ReclamationSousTypeType
        { reclamationSousTypeType_code :: Ds.ReclamationSousTypeCodeType
        , reclamationSousTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ReclamationSousTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ReclamationSousTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ReclamationSousTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ reclamationSousTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ reclamationSousTypeType_libelle x
            ]
 
data ReclamationTypeType = ReclamationTypeType
        { reclamationTypeType_code :: Ds.ReclamationTypeCodeType
        , reclamationTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ReclamationTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ReclamationTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ReclamationTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ reclamationTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ reclamationTypeType_libelle x
            ]
 
data RectificationMotifType = RectificationMotifType
        { rectificationMotifType_code :: Ds.RectificationMotifCodeType
        , rectificationMotifType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RectificationMotifType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RectificationMotifType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RectificationMotifType{} =
        toXMLElement s [ toXMLAttribute "code" $ rectificationMotifType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ rectificationMotifType_libelle x
            ]
 
data RelaisCommandeTypeType = RelaisCommandeTypeType
        { relaisCommandeTypeType_code :: Ds.RelaisCommandeTypeCodeType
        , relaisCommandeTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RelaisCommandeTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RelaisCommandeTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RelaisCommandeTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ relaisCommandeTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ relaisCommandeTypeType_libelle x
            ]
 
data RelaisNatureType = RelaisNatureType
        { relaisNatureType_code :: Ds.RelaisNatureCodeType
        , relaisNatureType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RelaisNatureType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RelaisNatureType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RelaisNatureType{} =
        toXMLElement s [ toXMLAttribute "code" $ relaisNatureType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ relaisNatureType_libelle x
            ]
 
data ReleveMediaType = ReleveMediaType
        { releveMediaType_code :: Ds.ReleveMediaCodeType
        , releveMediaType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ReleveMediaType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ReleveMediaType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ReleveMediaType{} =
        toXMLElement s [ toXMLAttribute "code" $ releveMediaType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ releveMediaType_libelle x
            ]
 
data ReleveModeType = ReleveModeType
        { releveModeType_code :: Ds.ReleveModeCodeType
        , releveModeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ReleveModeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ReleveModeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ReleveModeType{} =
        toXMLElement s [ toXMLAttribute "code" $ releveModeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ releveModeType_libelle x
            ]
 
data RelevePlageType = RelevePlageType
        { relevePlageType_code :: Ds.RelevePlageCodeType
        , relevePlageType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType RelevePlageType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (RelevePlageType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@RelevePlageType{} =
        toXMLElement s [ toXMLAttribute "code" $ relevePlageType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ relevePlageType_libelle x
            ]
 
data ReleveQualificationType = ReleveQualificationType
        { releveQualificationType_code :: Ds.ReleveQualificationCodeType
        , releveQualificationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ReleveQualificationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ReleveQualificationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ReleveQualificationType{} =
        toXMLElement s [ toXMLAttribute "code" $ releveQualificationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ releveQualificationType_libelle x
            ]
 
data ReleveTraitementModeType = ReleveTraitementModeType
        { releveTraitementModeType_code :: Ds.ReleveTraitementModeCodeType
        , releveTraitementModeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ReleveTraitementModeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ReleveTraitementModeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ReleveTraitementModeType{} =
        toXMLElement s [ toXMLAttribute "code" $ releveTraitementModeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ releveTraitementModeType_libelle x
            ]
 
data ResidenceTypeType = ResidenceTypeType
        { residenceTypeType_code :: Ds.ResidenceTypeCodeType
        , residenceTypeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ResidenceTypeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ResidenceTypeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ResidenceTypeType{} =
        toXMLElement s [ toXMLAttribute "code" $ residenceTypeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ residenceTypeType_libelle x
            ]
 
data SecteurGeographiqueType = SecteurGeographiqueType
        { secteurGeographiqueType_code :: Ds.SecteurGeographiqueCodeType
        , secteurGeographiqueType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType SecteurGeographiqueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (SecteurGeographiqueType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@SecteurGeographiqueType{} =
        toXMLElement s [ toXMLAttribute "code" $ secteurGeographiqueType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ secteurGeographiqueType_libelle x
            ]
 
data SituationContractuelleGestionModeType = SituationContractuelleGestionModeType
        { situationContractuelleGestionModeType_code :: Ds.SituationContractuelleGestionModeCodeType
        , situationContractuelleGestionModeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType SituationContractuelleGestionModeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (SituationContractuelleGestionModeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@SituationContractuelleGestionModeType{} =
        toXMLElement s [ toXMLAttribute "code" $ situationContractuelleGestionModeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ situationContractuelleGestionModeType_libelle x
            ]
 
data StructureComptageType = StructureComptageType
        { structureComptageType_code :: Ds.StructureComptageCodeType
        , structureComptageType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType StructureComptageType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (StructureComptageType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@StructureComptageType{} =
        toXMLElement s [ toXMLAttribute "code" $ structureComptageType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ structureComptageType_libelle x
            ]
 
data StructureTarifaireContexteUtilisationType = StructureTarifaireContexteUtilisationType
        { structureTarifaireContexteUtilisationType_code :: Ds.StructureTarifaireContexteUtilisationCodeType
        , structureTarifaireContexteUtilisationType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType StructureTarifaireContexteUtilisationType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (StructureTarifaireContexteUtilisationType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@StructureTarifaireContexteUtilisationType{} =
        toXMLElement s [ toXMLAttribute "code" $ structureTarifaireContexteUtilisationType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ structureTarifaireContexteUtilisationType_libelle x
            ]
 
data TensionLivraisonType = TensionLivraisonType
        { tensionLivraisonType_code :: Ds.TensionLivraisonCodeType
        , tensionLivraisonType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType TensionLivraisonType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (TensionLivraisonType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@TensionLivraisonType{} =
        toXMLElement s [ toXMLAttribute "code" $ tensionLivraisonType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ tensionLivraisonType_libelle x
            ]
 
data TensionType = TensionType
        { tensionType_valeur :: Ds.NbDecimalType
        , tensionType_unite :: Ds.TensionUniteSymboleType
        }
        deriving (Eq,Show)
instance SchemaType TensionType where
    parseSchemaType s = do
        (_,e) <- posnElement [s]
        commit $ interior e $ return TensionType
            `apply` parseSchemaType "valeur"
            `apply` parseSchemaType "unite"
    schemaTypeToXML s x@TensionType{} =
        toXMLElement s []
            [ schemaTypeToXML "valeur" $ tensionType_valeur x
            , schemaTypeToXML "unite" $ tensionType_unite x
            ]
 
data TourneeType = TourneeType
        { tourneeType_code :: Ds.TourneeCodeType
        , tourneeType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType TourneeType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (TourneeType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@TourneeType{} =
        toXMLElement s [ toXMLAttribute "code" $ tourneeType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ tourneeType_libelle x
            ]
 
data TransformateurCalibreType = TransformateurCalibreType
        { transformateurCalibreType_code :: Ds.TransformateurCalibreCodeType
        , transformateurCalibreType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType TransformateurCalibreType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (TransformateurCalibreType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@TransformateurCalibreType{} =
        toXMLElement s [ toXMLAttribute "code" $ transformateurCalibreType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ transformateurCalibreType_libelle x
            ]
 
data TransformateurCouplageType = TransformateurCouplageType
        { transformateurCouplageType_code :: Ds.TransformateurCouplageTypeCodeType
        , transformateurCouplageType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType TransformateurCouplageType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (TransformateurCouplageType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@TransformateurCouplageType{} =
        toXMLElement s [ toXMLAttribute "code" $ transformateurCouplageType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ transformateurCouplageType_libelle x
            ]
 
data TransformateurCourantPositionType = TransformateurCourantPositionType
        { transformateurCourantPositionType_code :: Ds.TransformateurCourantPositionCodeType
        , transformateurCourantPositionType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType TransformateurCourantPositionType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (TransformateurCourantPositionType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@TransformateurCourantPositionType{} =
        toXMLElement s [ toXMLAttribute "code" $ transformateurCourantPositionType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ transformateurCourantPositionType_libelle x
            ]
 
data TransformateurPrecisionClasseType = TransformateurPrecisionClasseType
        { transformateurPrecisionClasseType_code :: Ds.TransformateurPrecisionClasseCodeType
        , transformateurPrecisionClasseType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType TransformateurPrecisionClasseType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (TransformateurPrecisionClasseType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@TransformateurPrecisionClasseType{} =
        toXMLElement s [ toXMLAttribute "code" $ transformateurPrecisionClasseType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ transformateurPrecisionClasseType_libelle x
            ]
 
data UsageChantierType = UsageChantierType
        { usageChantierType_code :: Ds.UsageChantierCodeType
        , usageChantierType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType UsageChantierType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (UsageChantierType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@UsageChantierType{} =
        toXMLElement s [ toXMLAttribute "code" $ usageChantierType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ usageChantierType_libelle x
            ]
 
data ZoneQualiteDesserteType = ZoneQualiteDesserteType
        { zoneQualiteDesserteType_code :: Ds.ZoneQualiteDesserteCodeType
        , zoneQualiteDesserteType_libelle :: Maybe Ds.Chaine255Type
        }
        deriving (Eq,Show)
instance SchemaType ZoneQualiteDesserteType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "code" e pos
        commit $ interior e $ return (ZoneQualiteDesserteType a0)
            `apply` optional (parseSchemaType "libelle")
    schemaTypeToXML s x@ZoneQualiteDesserteType{} =
        toXMLElement s [ toXMLAttribute "code" $ zoneQualiteDesserteType_code x
                       ]
            [ maybe [] (schemaTypeToXML "libelle") $ zoneQualiteDesserteType_libelle x
            ]
