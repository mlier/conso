{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-|
Module      : Conso.Fr.Gaz.Adict.Types
Description : Types de données GRDF ADICT avec instances JSON

Définit l'ensemble des types Haskell correspondant aux objets JSON retournés
ou acceptés par l'API GRDF ADICT v2 (swagger B2B PROD v1.9).

Les noms de champs Haskell correspondent aux noms JSON (snake_case) via des
instances 'FromJSON' et 'ToJSON' explicites utilisant les clés du swagger.
-}
module Conso.Fr.Gaz.Adict.Types
  ( -- * Données de consommation (NDJSON)
    ConsoRestit(..)
  , Consommation(..)
  , ReleveDebut(..)
  , ReleveFin(..)
  , CoeffCalcul(..)
  , IndexValeur(..)
  , Periode(..)
  , BordereauPublication(..)
  , StatutRestitution(..)
    -- * Données d'injection (NDJSON)
  , InjectionRestit(..)
  , Injection(..)
    -- * Données contractuelles
  , RetourDonneesContractuelles(..)
  , DonneesContractuelles(..)
    -- * Données techniques
  , RetourDonneesTechniques(..)
  , DonneesTechniques(..)
  , SituationCompteurDetail(..)
  , PitdDetail(..)
    -- * Droits d'accès — énumérations
  , RoleTiers(..)
  , EtatDroitAcces(..)
  , StatutControlePreuve(..)
  , roleTiersText, roleTiersFromText
  , etatDroitAccesText, etatDroitAccesFromText
  , statutControlePreuveText, statutControlePreuveFromText
    -- * Droits d'accès — structures
  , DroitAcces(..)
  , DemandeAccesIn(..)
  , RetourDemandeAcces(..)
  , RetourFinAcces(..)
  , FiltreAcces(..)
    -- * Réponse générique traitement
  , RetourTraitement(..)
    -- * PCE
  , PceIdentifiant(..)
  ) where

import           Data.Aeson
import           Data.Text   ( Text )
import           GHC.Generics ( Generic )


-- ---------------------------------------------------------------------------
-- Helpers JSON

-- | Options aeson avec noms de champs inchangés (snake_case → snake_case).
aesonOpts :: Options
aesonOpts = defaultOptions { omitNothingFields = True }


-- ---------------------------------------------------------------------------
-- PCE

-- | Identifiant d'un PCE dans les réponses.
newtype PceIdentifiant = PceIdentifiant
    { id_pce :: Text
    } deriving (Show, Generic)

instance FromJSON PceIdentifiant
instance ToJSON   PceIdentifiant


-- ---------------------------------------------------------------------------
-- Statut de restitution

-- | Statut de restitution présent dans chaque ligne NDJSON de consommation/injection.
data StatutRestitution = StatutRestitution
    { sr_code    :: Maybe Text
    , sr_message :: Maybe Text
    } deriving (Show)

instance FromJSON StatutRestitution where
    parseJSON = withObject "StatutRestitution" $ \v -> StatutRestitution
        <$> v .:? "code"
        <*> v .:? "message"

instance ToJSON StatutRestitution where
    toJSON sr = object $ filter ((/= Null) . snd)
        [ "code"    .= sr_code    sr
        , "message" .= sr_message sr
        ]


-- ---------------------------------------------------------------------------
-- Coefficients de calcul

-- | Coefficients de conversion volume → énergie.
data CoeffCalcul = CoeffCalcul
    { coeff_pta        :: Maybe Double
    , valeur_pcs       :: Maybe Double
    , coeff_conversion :: Maybe Double
    } deriving (Show, Generic)

instance FromJSON CoeffCalcul
instance ToJSON   CoeffCalcul where toJSON = genericToJSON aesonOpts


-- ---------------------------------------------------------------------------
-- Index

-- | Valeur d'un index compteur avec horodate.
data IndexValeur = IndexValeur
    { valeur_index   :: Maybe Double
    , horodate_Index :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON IndexValeur
instance ToJSON   IndexValeur where toJSON = genericToJSON aesonOpts


-- ---------------------------------------------------------------------------
-- Période

-- | Période de consultation retournée dans les réponses de consommation.
data Periode = Periode
    { valeur     :: Maybe Text
    , date_debut :: Maybe Text
    , date_fin   :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Periode
instance ToJSON   Periode where toJSON = genericToJSON aesonOpts


-- ---------------------------------------------------------------------------
-- Bordereau de publication

data BordereauPublication = BordereauPublication
    { date_debut_bordereau :: Maybe Text
    , date_fin_bordereau   :: Maybe Text
    , nb_jour_gazier       :: Maybe Double
    } deriving (Show, Generic)

instance FromJSON BordereauPublication
instance ToJSON   BordereauPublication where toJSON = genericToJSON aesonOpts


-- ---------------------------------------------------------------------------
-- Relevé de début

data ReleveDebut = ReleveDebut
    { rd_date_releve           :: Maybe Text
    , rd_raison_releve         :: Maybe Text
    , rd_libelle_raison_releve :: Maybe Text
    , rd_qualite_releve        :: Maybe Text
    , rd_statut_releve         :: Maybe Text
    , rd_index_brut_debut      :: Maybe IndexValeur
    , rd_index_converti_debut  :: Maybe IndexValeur
    } deriving (Show)

instance FromJSON ReleveDebut where
    parseJSON = withObject "ReleveDebut" $ \v -> ReleveDebut
        <$> v .:? "date_releve"
        <*> v .:? "raison_releve"
        <*> v .:? "libelle_raison_releve"
        <*> v .:? "qualite_releve"
        <*> v .:? "statut_releve"
        <*> v .:? "index_brut_debut"
        <*> v .:? "index_converti_debut"

instance ToJSON ReleveDebut where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "date_releve"           .= rd_date_releve           r
        , "raison_releve"         .= rd_raison_releve         r
        , "libelle_raison_releve" .= rd_libelle_raison_releve r
        , "qualite_releve"        .= rd_qualite_releve        r
        , "statut_releve"         .= rd_statut_releve         r
        , "index_brut_debut"      .= rd_index_brut_debut      r
        , "index_converti_debut"  .= rd_index_converti_debut  r
        ]


-- ---------------------------------------------------------------------------
-- Relevé de fin

data ReleveFin = ReleveFin
    { rf_date_releve           :: Maybe Text
    , rf_raison_releve         :: Maybe Text
    , rf_libelle_raison_releve :: Maybe Text
    , rf_qualite_releve        :: Maybe Text
    , rf_statut_releve         :: Maybe Text
    , rf_index_brut_fin        :: Maybe IndexValeur
    , rf_index_converti_fin    :: Maybe IndexValeur
    } deriving (Show)

instance FromJSON ReleveFin where
    parseJSON = withObject "ReleveFin" $ \v -> ReleveFin
        <$> v .:? "date_releve"
        <*> v .:? "raison_releve"
        <*> v .:? "libelle_raison_releve"
        <*> v .:? "qualite_releve"
        <*> v .:? "statut_releve"
        <*> v .:? "index_brut_fin"
        <*> v .:? "index_converti_fin"

instance ToJSON ReleveFin where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "date_releve"           .= rf_date_releve           r
        , "raison_releve"         .= rf_raison_releve         r
        , "libelle_raison_releve" .= rf_libelle_raison_releve r
        , "qualite_releve"        .= rf_qualite_releve        r
        , "statut_releve"         .= rf_statut_releve         r
        , "index_brut_fin"        .= rf_index_brut_fin        r
        , "index_converti_fin"    .= rf_index_converti_fin    r
        ]


-- ---------------------------------------------------------------------------
-- Consommation

-- | Données de consommation d'une journée gazière.
data Consommation = Consommation
    { date_debut_consommation :: Maybe Text
    , date_fin_consommation   :: Maybe Text
    , flag_retour_zero        :: Maybe Bool
    , volume_brut             :: Maybe Double
    , coeff_calcul            :: Maybe CoeffCalcul
    , volume_converti         :: Maybe Double
    , energie                 :: Maybe Double
    , type_qualif_conso       :: Maybe Text
    , sens_flux_gaz           :: Maybe Text
    , statut_conso            :: Maybe Text
    , journee_gaziere         :: Maybe Text
    , type_conso              :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON Consommation
instance ToJSON   Consommation where toJSON = genericToJSON aesonOpts


-- ---------------------------------------------------------------------------
-- Réponse consommation (une ligne NDJSON)

-- | Un enregistrement NDJSON retourné par les endpoints de consommation
--   (publiées et informatives).
data ConsoRestit = ConsoRestit
    { cr_pce                :: Maybe PceIdentifiant
    , cr_periode            :: Maybe Periode
    , cr_releve_debut       :: Maybe ReleveDebut
    , cr_releve_fin         :: Maybe ReleveFin
    , cr_consommation       :: Maybe Consommation
    , cr_bordereau_pub      :: Maybe BordereauPublication
    , cr_statut_restitution :: Maybe StatutRestitution
    } deriving (Show)

instance FromJSON ConsoRestit where
    parseJSON = withObject "ConsoRestit" $ \v -> ConsoRestit
        <$> v .:? "pce"
        <*> v .:? "periode"
        <*> v .:? "releve_debut"
        <*> v .:? "releve_fin"
        <*> v .:? "consommation"
        <*> v .:? "bordereau_publication"
        <*> v .:? "statut_restitution"

instance ToJSON ConsoRestit where
    toJSON c = object $ filter ((/= Null) . snd)
        [ "pce"                  .= cr_pce                c
        , "periode"              .= cr_periode            c
        , "releve_debut"         .= cr_releve_debut       c
        , "releve_fin"           .= cr_releve_fin         c
        , "consommation"         .= cr_consommation       c
        , "bordereau_publication".= cr_bordereau_pub      c
        , "statut_restitution"   .= cr_statut_restitution c
        ]


-- ---------------------------------------------------------------------------
-- Injection

-- | Données d'injection d'une journée gazière.
data Injection = Injection
    { date_debut_injection :: Maybe Text
    , date_fin_injection   :: Maybe Text
    , inj_flag_retour_zero :: Maybe Bool
    , inj_volume_brut      :: Maybe Double
    , inj_coeff_calcul     :: Maybe CoeffCalcul
    , inj_volume_converti  :: Maybe Double
    , inj_energie          :: Maybe Double
    , type_qualif_injection :: Maybe Text
    , inj_sens_flux_gaz    :: Maybe Text
    , statut_injection     :: Maybe Text
    , inj_journee_gaziere  :: Maybe Text
    , type_injection       :: Maybe Text
    } deriving (Show)

instance FromJSON Injection where
    parseJSON = withObject "Injection" $ \v -> Injection
        <$> v .:? "date_debut_injection"
        <*> v .:? "date_fin_injection"
        <*> v .:? "flag_retour_zero"
        <*> v .:? "volume_brut"
        <*> v .:? "coeff_calcul"
        <*> v .:? "volume_converti"
        <*> v .:? "energie"
        <*> v .:? "type_qualif_injection"
        <*> v .:? "sens_flux_gaz"
        <*> v .:? "statut_injection"
        <*> v .:? "journee_gaziere"
        <*> v .:? "type_injection"

instance ToJSON Injection where
    toJSON inj = object $ filter ((/= Null) . snd)
        [ "date_debut_injection"  .= date_debut_injection  inj
        , "date_fin_injection"    .= date_fin_injection    inj
        , "flag_retour_zero"      .= inj_flag_retour_zero  inj
        , "volume_brut"           .= inj_volume_brut       inj
        , "coeff_calcul"          .= inj_coeff_calcul      inj
        , "volume_converti"       .= inj_volume_converti   inj
        , "energie"               .= inj_energie           inj
        , "type_qualif_injection" .= type_qualif_injection inj
        , "sens_flux_gaz"         .= inj_sens_flux_gaz     inj
        , "statut_injection"      .= statut_injection      inj
        , "journee_gaziere"       .= inj_journee_gaziere   inj
        , "type_injection"        .= type_injection        inj
        ]

-- | Un enregistrement NDJSON retourné par l'endpoint d'injection publiée.
data InjectionRestit = InjectionRestit
    { ir_pce                :: Maybe PceIdentifiant
    , ir_periode            :: Maybe Periode
    , ir_releve_debut       :: Maybe ReleveDebut
    , ir_releve_fin         :: Maybe ReleveFin
    , ir_injection          :: Maybe Injection
    , ir_bordereau_pub      :: Maybe BordereauPublication
    , ir_statut_restitution :: Maybe StatutRestitution
    } deriving (Show)

instance FromJSON InjectionRestit where
    parseJSON = withObject "InjectionRestit" $ \v -> InjectionRestit
        <$> v .:? "pce"
        <*> v .:? "periode"
        <*> v .:? "releve_debut"
        <*> v .:? "releve_fin"
        <*> v .:? "injection"
        <*> v .:? "bordereau_publication"
        <*> v .:? "statut_restitution"

instance ToJSON InjectionRestit where
    toJSON ir = object $ filter ((/= Null) . snd)
        [ "pce"                   .= ir_pce                ir
        , "periode"               .= ir_periode            ir
        , "releve_debut"          .= ir_releve_debut       ir
        , "releve_fin"            .= ir_releve_fin         ir
        , "injection"             .= ir_injection          ir
        , "bordereau_publication" .= ir_bordereau_pub      ir
        , "statut_restitution"    .= ir_statut_restitution ir
        ]


-- ---------------------------------------------------------------------------
-- Données contractuelles

-- | Données contractuelles d'un PCE.
data DonneesContractuelles = DonneesContractuelles
    { dc_date_mes                         :: Maybe Text
    , dc_tarif_acheminement               :: Maybe Text
    , dc_date_publication                 :: Maybe Text
    , dc_consommation_journaliere_plafond :: Maybe Text
    , dc_car                              :: Maybe Value
    , dc_cja                              :: Maybe Value
    , dc_profil                           :: Maybe Value
    , dc_modulation                       :: Maybe Value
    } deriving (Show)

instance FromJSON DonneesContractuelles where
    parseJSON = withObject "DonneesContractuelles" $ \v -> DonneesContractuelles
        <$> v .:? "date_mes"
        <*> v .:? "tarif_acheminement"
        <*> v .:? "date_publication"
        <*> v .:? "consommation_journaliere_plafond"
        <*> v .:? "car"
        <*> v .:? "cja"
        <*> v .:? "profil"
        <*> v .:? "modulation"

instance ToJSON DonneesContractuelles where
    toJSON dc = object $ filter ((/= Null) . snd)
        [ "date_mes"                         .= dc_date_mes                         dc
        , "tarif_acheminement"               .= dc_tarif_acheminement               dc
        , "date_publication"                 .= dc_date_publication                 dc
        , "consommation_journaliere_plafond" .= dc_consommation_journaliere_plafond dc
        , "car"                              .= dc_car                              dc
        , "cja"                              .= dc_cja                              dc
        , "profil"                           .= dc_profil                           dc
        , "modulation"                       .= dc_modulation                       dc
        ]

-- | Réponse de l'endpoint @GET /pce/{id_pce}/donnees_contractuelles@.
data RetourDonneesContractuelles = RetourDonneesContractuelles
    { rdc_pce                :: Maybe PceIdentifiant
    , rdc_donnees            :: Maybe DonneesContractuelles
    , rdc_statut_restitution :: Maybe Value
    } deriving (Show)

instance FromJSON RetourDonneesContractuelles where
    parseJSON = withObject "RetourDonneesContractuelles" $ \v -> RetourDonneesContractuelles
        <$> v .:? "pce"
        <*> v .:? "donnees_contractuelles"
        <*> v .:? "statut_restitution"

instance ToJSON RetourDonneesContractuelles where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "pce"                  .= rdc_pce                r
        , "donnees_contractuelles" .= rdc_donnees          r
        , "statut_restitution"   .= rdc_statut_restitution r
        ]


-- ---------------------------------------------------------------------------
-- Données techniques

-- | Adresse physique du compteur.
data SituationCompteurDetail = SituationCompteurDetail
    { numero_rue         :: Maybe Text
    , nom_rue            :: Maybe Text
    , complement_adresse :: Maybe Text
    , scd_code_postal    :: Maybe Text
    , commune            :: Maybe Text
    } deriving (Show)

instance FromJSON SituationCompteurDetail where
    parseJSON = withObject "SituationCompteurDetail" $ \v -> SituationCompteurDetail
        <$> v .:? "numero_rue"
        <*> v .:? "nom_rue"
        <*> v .:? "complement_adresse"
        <*> v .:? "code_postal"
        <*> v .:? "commune"

instance ToJSON SituationCompteurDetail where
    toJSON s = object $ filter ((/= Null) . snd)
        [ "numero_rue"         .= numero_rue         s
        , "nom_rue"            .= nom_rue             s
        , "complement_adresse" .= complement_adresse  s
        , "code_postal"        .= scd_code_postal     s
        , "commune"            .= commune             s
        ]

-- | Identifiant du Périmètre d'Injection et de Transit de Distribution.
data PitdDetail = PitdDetail
    { identifiant_pitd :: Maybe Text
    , libelle_pitd     :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON PitdDetail
instance ToJSON   PitdDetail where toJSON = genericToJSON aesonOpts

-- | Données techniques d'un PCE.
data DonneesTechniques = DonneesTechniques
    { dt_situation_compteur       :: Maybe SituationCompteurDetail
    , dt_caracteristiques_compteur :: Maybe Value
    , dt_pitd                     :: Maybe PitdDetail
    , dt_regime_propriete         :: Maybe Value
    } deriving (Show)

instance FromJSON DonneesTechniques where
    parseJSON = withObject "DonneesTechniques" $ \v -> DonneesTechniques
        <$> v .:? "situation_compteur"
        <*> v .:? "caracteristiques_compteur"
        <*> v .:? "pitd"
        <*> v .:? "regime_propriete"

instance ToJSON DonneesTechniques where
    toJSON dt = object $ filter ((/= Null) . snd)
        [ "situation_compteur"        .= dt_situation_compteur        dt
        , "caracteristiques_compteur" .= dt_caracteristiques_compteur dt
        , "pitd"                      .= dt_pitd                      dt
        , "regime_propriete"          .= dt_regime_propriete          dt
        ]

-- | Réponse de l'endpoint @GET /pce/{id_pce}/donnees_techniques@.
data RetourDonneesTechniques = RetourDonneesTechniques
    { rdt_pce                :: Maybe PceIdentifiant
    , rdt_donnees            :: Maybe DonneesTechniques
    , rdt_statut_restitution :: Maybe Value
    } deriving (Show)

instance FromJSON RetourDonneesTechniques where
    parseJSON = withObject "RetourDonneesTechniques" $ \v -> RetourDonneesTechniques
        <$> v .:? "pce"
        <*> v .:? "donnees_techniques"
        <*> v .:? "statut_restitution"

instance ToJSON RetourDonneesTechniques where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "pce"                .= rdt_pce                r
        , "donnees_techniques" .= rdt_donnees            r
        , "statut_restitution" .= rdt_statut_restitution r
        ]


-- ---------------------------------------------------------------------------
-- Droits d'accès — énumérations

-- | Rôle du tiers dans un droit d'accès.
data RoleTiers
    = AutoriseContratFourniture
    | DetenteurContratFourniture
    | AutoriseContratInjection
    | DetenteurContratInjection
    | AutreRoleTiers Text
    deriving (Show, Eq)

roleTiersText :: RoleTiers -> Text
roleTiersText AutoriseContratFourniture  = "AUTORISE_CONTRAT_FOURNITURE"
roleTiersText DetenteurContratFourniture = "DETENTEUR_CONTRAT_FOURNITURE"
roleTiersText AutoriseContratInjection   = "AUTORISE_CONTRAT_INJECTION"
roleTiersText DetenteurContratInjection  = "DETENTEUR_CONTRAT_INJECTION"
roleTiersText (AutreRoleTiers t)         = t

roleTiersFromText :: Text -> RoleTiers
roleTiersFromText "AUTORISE_CONTRAT_FOURNITURE"  = AutoriseContratFourniture
roleTiersFromText "DETENTEUR_CONTRAT_FOURNITURE" = DetenteurContratFourniture
roleTiersFromText "AUTORISE_CONTRAT_INJECTION"   = AutoriseContratInjection
roleTiersFromText "DETENTEUR_CONTRAT_INJECTION"  = DetenteurContratInjection
roleTiersFromText t                              = AutreRoleTiers t

instance FromJSON RoleTiers where parseJSON = fmap roleTiersFromText . parseJSON
instance ToJSON   RoleTiers where toJSON    = toJSON . roleTiersText


-- | État d'un droit d'accès (valeurs API GRDF PROD v1.9).
data EtatDroitAcces
    = EtatActive
    | EtatAValider
    | EtatRevoquee
    | EtatAReverifier
    | EtatObsolete
    | EtatRefusee
    | AutreEtat Text
    deriving (Show, Eq)

etatDroitAccesText :: EtatDroitAcces -> Text
etatDroitAccesText EtatActive      = "Actif"
etatDroitAccesText EtatAValider    = "A valider"
etatDroitAccesText EtatRevoquee    = "Révoqué"
etatDroitAccesText EtatAReverifier = "A revérifier"
etatDroitAccesText EtatObsolete    = "Obsolète"
etatDroitAccesText EtatRefusee     = "Refusé"
etatDroitAccesText (AutreEtat t)   = t

etatDroitAccesFromText :: Text -> EtatDroitAcces
etatDroitAccesFromText "actif"       = EtatActive
etatDroitAccesFromText "avalider"    = EtatAValider
etatDroitAccesFromText "revoque"     = EtatRevoquee
etatDroitAccesFromText "areverifier" = EtatAReverifier
etatDroitAccesFromText "obsolete"     = EtatObsolete
etatDroitAccesFromText "refuse"      = EtatRefusee
etatDroitAccesFromText t              = AutreEtat t

instance FromJSON EtatDroitAcces where parseJSON = fmap etatDroitAccesFromText . parseJSON
instance ToJSON   EtatDroitAcces where toJSON    = toJSON . etatDroitAccesText


-- | Statut du contrôle de preuve d'un droit d'accès (valeurs API GRDF PROD v1.9).
data StatutControlePreuve
    = PreuveEnAttente
    | PreuveEnCoursDeVerification
    | PreuveVerifieeOK
    | PreuveVerifieeKO
    | AutreStatut Text
    deriving (Show, Eq)

statutControlePreuveText :: StatutControlePreuve -> Text
statutControlePreuveText PreuveEnAttente             = "Preuve en attente"
statutControlePreuveText PreuveEnCoursDeVerification = "Preuve en cours de vérification"
statutControlePreuveText PreuveVerifieeOK            = "Preuve Vérifiée OK"
statutControlePreuveText PreuveVerifieeKO            = "Preuve Vérifiée KO"
statutControlePreuveText (AutreStatut t)             = t

statutControlePreuveFromText :: Text -> StatutControlePreuve
statutControlePreuveFromText "attente"               = PreuveEnAttente
statutControlePreuveFromText "verification"          = PreuveEnCoursDeVerification
statutControlePreuveFromText "verifieeok"            = PreuveVerifieeOK
statutControlePreuveFromText "verifieeko"            = PreuveVerifieeKO
statutControlePreuveFromText t                       = AutreStatut t

instance FromJSON StatutControlePreuve where parseJSON = fmap statutControlePreuveFromText . parseJSON
instance ToJSON   StatutControlePreuve where toJSON    = toJSON . statutControlePreuveText


-- ---------------------------------------------------------------------------
-- Droits d'accès — structures

-- | Droit d'accès retourné par @GET /droits_acces@ (ligne NDJSON).
data DroitAcces = DroitAcces
    { da_id_droit_acces                   :: Maybe Text
    , da_id_pce                           :: Maybe Text
    , da_role_tiers                       :: Maybe RoleTiers
    , da_raison_sociale_du_tiers          :: Maybe Text
    , da_nom_titulaire                    :: Maybe Text
    , da_raison_sociale_du_titulaire      :: Maybe Text
    , da_courriel_titulaire               :: Maybe Text
    , da_numero_telephone_mobile_titulaire :: Maybe Text
    , da_code_postal                      :: Maybe Text
    , da_date_debut_droit_acces           :: Maybe Text
    , da_date_fin_droit_acces             :: Maybe Text
    , da_perim_donnees_conso_debut        :: Maybe Text
    , da_perim_donnees_conso_fin          :: Maybe Text
    , da_perim_donnees_inj_debut          :: Maybe Text
    , da_perim_donnees_inj_fin            :: Maybe Text
    , da_perim_donnees_contractuelles     :: Maybe Text
    , da_perim_donnees_techniques         :: Maybe Text
    , da_perim_donnees_informatives       :: Maybe Text
    , da_perim_donnees_publiees           :: Maybe Text
    , da_date_creation                    :: Maybe Text
    , da_etat_droit_acces                 :: Maybe EtatDroitAcces
    , da_date_revocation                  :: Maybe Text
    , da_source_revocation                :: Maybe Text
    , da_date_passage_a_obsolete          :: Maybe Text
    , da_source_passage_a_obsolete        :: Maybe Text
    , da_date_passage_a_refuse            :: Maybe Text
    , da_source_passage_a_refuse          :: Maybe Text
    , da_parcours                         :: Maybe Text
    , da_statut_controle_preuve           :: Maybe StatutControlePreuve
    , da_date_limite_transmission_preuve  :: Maybe Text
    } deriving (Show)

instance FromJSON DroitAcces where
    parseJSON = withObject "DroitAcces" $ \v -> DroitAcces
        <$> v .:? "id_droit_acces"
        <*> v .:? "id_pce"
        <*> v .:? "role_tiers"
        <*> v .:? "raison_sociale_du_tiers"
        <*> v .:? "nom_titulaire"
        <*> v .:? "raison_sociale_du_titulaire"
        <*> v .:? "courriel_titulaire"
        <*> v .:? "numero_telephone_mobile_titulaire"
        <*> v .:? "code_postal"
        <*> v .:? "date_debut_droit_acces"
        <*> v .:? "date_fin_droit_acces"
        <*> v .:? "perim_donnees_conso_debut"
        <*> v .:? "perim_donnees_conso_fin"
        <*> v .:? "perim_donnees_inj_debut"
        <*> v .:? "perim_donnees_inj_fin"
        <*> v .:? "perim_donnees_contractuelles"
        <*> v .:? "perim_donnees_techniques"
        <*> v .:? "perim_donnees_informatives"
        <*> v .:? "perim_donnees_publiees"
        <*> v .:? "date_creation"
        <*> v .:? "etat_droit_acces"
        <*> v .:? "date_revocation"
        <*> v .:? "source_revocation"
        <*> v .:? "date_passage_a_obsolete"
        <*> v .:? "source_passage_a_obsolete"
        <*> v .:? "date_passage_a_refuse"
        <*> v .:? "source_passage_a_refuse"
        <*> v .:? "parcours"
        <*> v .:? "statut_controle_preuve"
        <*> v .:? "date_limite_transmission_preuve"

instance ToJSON DroitAcces where
    toJSON da = object $ filter ((/= Null) . snd)
        [ "id_droit_acces"                    .= da_id_droit_acces                   da
        , "id_pce"                            .= da_id_pce                           da
        , "role_tiers"                        .= da_role_tiers                       da
        , "raison_sociale_du_tiers"           .= da_raison_sociale_du_tiers          da
        , "nom_titulaire"                     .= da_nom_titulaire                    da
        , "raison_sociale_du_titulaire"       .= da_raison_sociale_du_titulaire      da
        , "courriel_titulaire"                .= da_courriel_titulaire               da
        , "numero_telephone_mobile_titulaire" .= da_numero_telephone_mobile_titulaire da
        , "code_postal"                       .= da_code_postal                      da
        , "date_debut_droit_acces"            .= da_date_debut_droit_acces           da
        , "date_fin_droit_acces"              .= da_date_fin_droit_acces             da
        , "perim_donnees_conso_debut"         .= da_perim_donnees_conso_debut        da
        , "perim_donnees_conso_fin"           .= da_perim_donnees_conso_fin          da
        , "perim_donnees_inj_debut"           .= da_perim_donnees_inj_debut          da
        , "perim_donnees_inj_fin"             .= da_perim_donnees_inj_fin            da
        , "perim_donnees_contractuelles"      .= da_perim_donnees_contractuelles     da
        , "perim_donnees_techniques"          .= da_perim_donnees_techniques         da
        , "perim_donnees_informatives"        .= da_perim_donnees_informatives       da
        , "perim_donnees_publiees"            .= da_perim_donnees_publiees           da
        , "date_creation"                     .= da_date_creation                    da
        , "etat_droit_acces"                  .= da_etat_droit_acces                 da
        , "date_revocation"                   .= da_date_revocation                  da
        , "source_revocation"                 .= da_source_revocation                da
        , "date_passage_a_obsolete"           .= da_date_passage_a_obsolete          da
        , "source_passage_a_obsolete"         .= da_source_passage_a_obsolete        da
        , "date_passage_a_refuse"             .= da_date_passage_a_refuse            da
        , "source_passage_a_refuse"           .= da_source_passage_a_refuse          da
        , "parcours"                          .= da_parcours                         da
        , "statut_controle_preuve"            .= da_statut_controle_preuve           da
        , "date_limite_transmission_preuve"   .= da_date_limite_transmission_preuve  da
        ]


-- | Corps de la requête @PUT /pce/{id_pce}/droit_acces@.
data DemandeAccesIn = DemandeAccesIn
    { din_role_tiers                          :: Text
    , din_raison_sociale                      :: Maybe Text
    , din_nom_titulaire                       :: Maybe Text
    , din_code_postal                         :: Text
    , din_courriel_titulaire                  :: Maybe Text
    , din_numero_telephone_mobile_titulaire   :: Maybe Text
    , din_date_debut_droit_acces              :: Maybe Text
    , din_date_fin_droit_acces                :: Maybe Text
    , din_perim_donnees_conso_debut           :: Maybe Text
    , din_perim_donnees_conso_fin             :: Maybe Text
    , din_perim_donnees_inj_debut             :: Maybe Text
    , din_perim_donnees_inj_fin               :: Maybe Text
    , din_perim_donnees_contractuelles        :: Maybe Text
    , din_perim_donnees_techniques            :: Maybe Text
    , din_perim_donnees_informatives          :: Maybe Text
    , din_perim_donnees_publiees              :: Maybe Text
    } deriving (Show)

instance ToJSON DemandeAccesIn where
    toJSON d = object $ filter ((/= Null) . snd)
        [ "role_tiers"                          .= din_role_tiers                        d
        , "raison_sociale"                      .= din_raison_sociale                    d
        , "nom_titulaire"                       .= din_nom_titulaire                     d
        , "code_postal"                         .= din_code_postal                       d
        , "courriel_titulaire"                  .= din_courriel_titulaire                d
        , "numero_telephone_mobile_titulaire"   .= din_numero_telephone_mobile_titulaire d
        , "date_debut_droit_acces"              .= din_date_debut_droit_acces            d
        , "date_fin_droit_acces"                .= din_date_fin_droit_acces              d
        , "perim_donnees_conso_debut"           .= din_perim_donnees_conso_debut         d
        , "perim_donnees_conso_fin"             .= din_perim_donnees_conso_fin           d
        , "perim_donnees_inj_debut"             .= din_perim_donnees_inj_debut           d
        , "perim_donnees_inj_fin"               .= din_perim_donnees_inj_fin             d
        , "perim_donnees_contractuelles"        .= din_perim_donnees_contractuelles      d
        , "perim_donnees_techniques"            .= din_perim_donnees_techniques          d
        , "perim_donnees_informatives"          .= din_perim_donnees_informatives        d
        , "perim_donnees_publiees"              .= din_perim_donnees_publiees            d
        ]

instance FromJSON DemandeAccesIn where
    parseJSON = withObject "DemandeAccesIn" $ \v -> DemandeAccesIn
        <$> v .:  "role_tiers"
        <*> v .:? "raison_sociale"
        <*> v .:? "nom_titulaire"
        <*> v .:  "code_postal"
        <*> v .:? "courriel_titulaire"
        <*> v .:? "numero_telephone_mobile_titulaire"
        <*> v .:? "date_debut_droit_acces"
        <*> v .:? "date_fin_droit_acces"
        <*> v .:? "perim_donnees_conso_debut"
        <*> v .:? "perim_donnees_conso_fin"
        <*> v .:? "perim_donnees_inj_debut"
        <*> v .:? "perim_donnees_inj_fin"
        <*> v .:? "perim_donnees_contractuelles"
        <*> v .:? "perim_donnees_techniques"
        <*> v .:? "perim_donnees_informatives"
        <*> v .:? "perim_donnees_publiees"


-- | Réponse à une déclaration de droit d'accès (201 Créé).
data RetourDemandeAcces = RetourDemandeAcces
    { rda_code_statut_traitement    :: Maybe Text
    , rda_message_retour_traitement :: Maybe Text
    , rda_id_droit_acces            :: Maybe Text
    } deriving (Show)

instance FromJSON RetourDemandeAcces where
    parseJSON = withObject "RetourDemandeAcces" $ \v -> RetourDemandeAcces
        <$> v .:? "code_statut_traitement"
        <*> v .:? "message_retour_traitement"
        <*> v .:? "id_droit_acces"

instance ToJSON RetourDemandeAcces where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "code_statut_traitement"    .= rda_code_statut_traitement    r
        , "message_retour_traitement" .= rda_message_retour_traitement r
        , "id_droit_acces"            .= rda_id_droit_acces            r
        ]


-- | Réponse à une révocation de droit d'accès.
data RetourFinAcces = RetourFinAcces
    { rfa_code_statut_traitement    :: Maybe Text
    , rfa_message_retour_traitement :: Maybe Text
    } deriving (Show)

instance FromJSON RetourFinAcces where
    parseJSON = withObject "RetourFinAcces" $ \v -> RetourFinAcces
        <$> v .:? "code_statut_traitement"
        <*> v .:? "message_retour_traitement"

instance ToJSON RetourFinAcces where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "code_statut_traitement"    .= rfa_code_statut_traitement    r
        , "message_retour_traitement" .= rfa_message_retour_traitement r
        ]


-- | Corps de la requête @POST /droits_acces@ (filtres de recherche).
-- Les champs sont des listes car l'API attend des tableaux JSON.
data FiltreAcces = FiltreAcces
    { fa_role_tiers             :: [RoleTiers]
    , fa_id_pce                 :: [Text]
    , fa_statut_controle_preuve :: [StatutControlePreuve]
    , fa_etat_droit_acces       :: [EtatDroitAcces]
    } deriving (Show)

instance ToJSON FiltreAcces where
    toJSON f = object $ filter ((/= Null) . snd)
        [ "role_tiers"             .= nullIfEmpty (fa_role_tiers             f)
        , "id_pce"                 .= nullIfEmpty (fa_id_pce                 f)
        , "statut_controle_preuve" .= nullIfEmpty (fa_statut_controle_preuve f)
        , "etat_droit_acces"       .= nullIfEmpty (fa_etat_droit_acces       f)
        ]
      where
        nullIfEmpty [] = Null
        nullIfEmpty xs = toJSON xs

instance FromJSON FiltreAcces where
    parseJSON = withObject "FiltreAcces" $ \v -> FiltreAcces
        <$> v .:? "role_tiers"             .!= []
        <*> v .:? "id_pce"                 .!= []
        <*> v .:? "statut_controle_preuve" .!= []
        <*> v .:? "etat_droit_acces"       .!= []


-- | Réponse générique avec code et message de traitement.
data RetourTraitement = RetourTraitement
    { rt_code_statut_traitement    :: Maybe Text
    , rt_message_retour_traitement :: Maybe Text
    } deriving (Show)

instance FromJSON RetourTraitement where
    parseJSON = withObject "RetourTraitement" $ \v -> RetourTraitement
        <$> v .:? "code_statut_traitement"
        <*> v .:? "message_retour_traitement"

instance ToJSON RetourTraitement where
    toJSON r = object $ filter ((/= Null) . snd)
        [ "code_statut_traitement"    .= rt_code_statut_traitement    r
        , "message_retour_traitement" .= rt_message_retour_traitement r
        ]


