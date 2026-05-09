{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Conso.Fr.Elec.SiteDB.Types.C68 where

import           Data.Text           (Text)
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import qualified Data.Aeson.Key      as Key
import qualified Data.Vector         as V
import           Conso.Fr.Elec.SiteDB.Types.Common (PrmId(..))

data CalendrierFournisseur = CalendrierFournisseur
  { cfCode                   :: Text
  , cfPeriodeMobileAutorisee :: Maybe Bool
  , cfProfilable             :: Maybe Bool
  } deriving (Show, Eq)

data ContinuiteFourniture = ContinuiteFourniture
  { contIdFonctionnel     :: Maybe Text
  , contType              :: Maybe Text
  , contPeriodicite       :: Maybe Text
  , contDateReference     :: Maybe Text
  , contMoisReference     :: Maybe Int
  , contNbCoupuresBreves  :: Maybe Int
  , contNbCoupuresLongues :: Maybe Int
  , contNbTotalCoupures   :: Maybe Int
  , contTypeCoupures      :: Maybe Text
  } deriving (Show, Eq)

data QualiteFourniture = QualiteFourniture
  { qualIdFonctionnel   :: Maybe Text
  , qualPeriodicite     :: Maybe Text
  , qualDateReference   :: Maybe Text
  , qualNbCreux         :: Maybe Double
  , qualProfondeurCreux :: Maybe Double
  , qualDureeValeur     :: Maybe Double
  , qualDureeUnite      :: Maybe Text
  } deriving (Show, Eq)

data InfoTechniqueContractuelle = InfoTechniqueContractuelle
  { idPrm                        :: PrmId
  , segment                      :: Maybe Text
  , etatContractuel              :: Maybe Text
  , etatAlimentation             :: Maybe Text
  , puissanceSouscrite           :: Maybe Text
  , domaineTension               :: Maybe Text
  -- adresse
  , adresseNumeroNomVoie         :: Maybe Text
  , adresseBatiment              :: Maybe Text
  , adresseEscalierEtage         :: Maybe Text
  , adresseLieuDit               :: Maybe Text
  , adresseCodePostal            :: Maybe Text
  , adresseCommune               :: Maybe Text
  -- typage
  , typageSensible               :: Maybe Bool
  , typageAlimComplementaire     :: Maybe Bool
  , typageAlimSecours            :: Maybe Bool
  , typageBornePoste             :: Maybe Bool
  , typageBorneFixe              :: Maybe Bool
  -- synthèse contractuelle
  , niveauOuvertureServices      :: Maybe Text
  , dateModifFta                 :: Maybe Text
  , dateAugmentationPuissance    :: Maybe Text
  , dateDiminutionPuissance      :: Maybe Text
  , dateMesSoutirage             :: Maybe Text
  , dateMesInjection             :: Maybe Text
  , datePremierePoseLinky        :: Maybe Text
  , telephoneDepannage           :: Maybe Text
  , autoConsoCollective          :: Maybe Text
  , autoConsoIndividuelle        :: Maybe Text
  -- structure tarifaire (sit contractuelle [0])
  , puissanceSouscriteUnite      :: Maybe Text
  , formuleTarifaireCode         :: Maybe Text
  , formuleTarifaireLibelle      :: Maybe Text
  , codeTarifAcheminement        :: Maybe Text
  , typeOffre                    :: Maybe Text
  , contexteUtilisation          :: Maybe Text
  , forfaitValeur                :: Maybe Text
  , forfaitUnite                 :: Maybe Text
  , calendrierTurpeCode          :: Maybe Text
  , groupePeriodeMobile          :: Maybe Text
  , groupePeriodeMobileDistrib   :: Maybe Text
  , dateDebutContrat             :: Maybe Text
  , natureContrat                :: Maybe Text
  , typeInjection                :: Maybe Bool
  , refusPoseAmm                 :: Maybe Bool
  , dateRefusPoseAmm             :: Maybe Text
  -- client final
  , categorieClient              :: Maybe Text
  , typeResidence                :: Maybe Text
  , referenceClient              :: Maybe Text
  , titulaireCivilite            :: Maybe Text
  , titulaireNom                 :: Maybe Text
  , titulairePrenom              :: Maybe Text
  , titulaireDenominationSociale :: Maybe Text
  , titulaireNomCommercial       :: Maybe Text
  , titulaireSiren               :: Maybe Text
  , titulaireSiret               :: Maybe Text
  , titulaireSecteur             :: Maybe Text
  , titulaireActiviteNaf         :: Maybe Text
  , referenceContrat             :: Maybe Text
  -- situation alimentation
  , tensionLivraison             :: Maybe Text
  , puissanceRaccordSoutirage    :: Maybe Text
  , puissanceRaccordInjection    :: Maybe Text
  , puissanceLimiteSoutirage     :: Maybe Text
  , tensionContractuelle         :: Maybe Text
  , modeAlimApresCompteur        :: Maybe Text
  , nbFilsBranchement            :: Maybe Int
  , zoneQualiteDesserte          :: Maybe Text
  , longueurLiaisonAerienne      :: Maybe Text
  , longueurLiaisonSouterraine   :: Maybe Text
  , prodAutonomeNb               :: Maybe Int
  , prodAutonomePuissance        :: Maybe Text
  , coupureLocalisation          :: Maybe Text
  , coupureRestrictionMotif      :: Maybe Text
  , limiteurPuissance            :: Maybe Text
  -- dispositif de comptage
  , typeComptage                 :: Maybe Text
  , modeReleve                   :: Maybe Text
  , mediaReleve                  :: Maybe Text
  , teleoperable                 :: Maybe Bool
  , eligiblePeriodeMobile        :: Maybe Bool
  , tensionComptage              :: Maybe Text
  , comptageParticularite        :: Maybe Text
  , boitierTelereport            :: Maybe Bool
  -- compteur principal [0]
  , matriculeCompteur            :: Maybe Text
  , numeroSerieCompteur          :: Maybe Text
  , ticActivee                   :: Maybe Bool
  , ticActivable                 :: Maybe Bool
  , ticStandard                  :: Maybe Bool
  , periodeDeploiementLinky      :: Maybe Text
  , intensiteNominale            :: Maybe Text
  , puissanceMaxCompteur         :: Maybe Text
  , coefficientLecture           :: Maybe Double
  , nbFilsCompteur               :: Maybe Int
  , regimeProprieteCompteur      :: Maybe Text
  , compteurAccessibilite        :: Maybe Bool
  , compteurSituation            :: Maybe Text
  -- disjoncteur
  , disjoncteurCalibre           :: Maybe Text
  , disjoncteurNature            :: Maybe Text
  , disjoncteurNbPoles           :: Maybe Int
  , disjoncteurAccessibilite     :: Maybe Bool
  , disjoncteurSituation         :: Maybe Text
  , disjoncteurIntensiteReglage  :: Maybe Text
  , disjoncteurRegimePropriete   :: Maybe Text
  -- TC
  , tcCalibre                    :: Maybe Text
  , tcClassePrecision            :: Maybe Text
  , tcCouplage                   :: Maybe Text
  , tcPosition                   :: Maybe Text
  , tcRegimePropriete            :: Maybe Text
  -- TT
  , ttCalibre                    :: Maybe Text
  , ttClassePrecision            :: Maybe Text
  , ttCouplage                   :: Maybe Text
  -- pertes
  , pertesFer                    :: Maybe Double
  , pertesJoules                 :: Maybe Double
  , pertesReactives              :: Maybe Double
  -- relais
  , relaisNature                 :: Maybe Text
  , relaisPlageHc                :: Maybe Text
  , relaisTypeCommande           :: Maybe Text
  , relaisRegimePropriete        :: Maybe Text
  -- production [0]
  , productionFiliere            :: Maybe Text
  , productionTechnologie        :: Maybe Text
  -- listes enfants
  , calendriersFournisseur       :: [CalendrierFournisseur]
  , continuiteFourniture         :: [ContinuiteFourniture]
  , qualiteFourniture            :: [QualiteFourniture]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Instance FromJSON

instance FromJSON InfoTechniqueContractuelle where
  parseJSON = withObject "C68Item" parseC68Item

parseC68Item :: Object -> Parser InfoTechniqueContractuelle
parseC68Item o = do
  pid   <- o .: "idPrm"
  seg   <- firstSitContractuelle o "segment"
  etat  <- firstSitContractuelleNested o ["informationsContractuelles", "etatContractuel"]
  etatAl <- situationAlim o "etatAlimentation"
  puiss  <- firstSitContractuelleNested o ["structureTarifaire", "puissanceSouscrite", "valeur"]
  domTen <- situationAlimNested o ["alimentationPrincipale", "domaineTension"]

  -- adresse
  addrNomVoie  <- donneesGeneralesNested o ["adresseInstallationNonNormalisee", "numeroEtNomVoie"]
  addrBat      <- donneesGeneralesNested o ["adresseInstallationNonNormalisee", "batiment"]
  addrEsc      <- donneesGeneralesNested o ["adresseInstallationNonNormalisee", "escalierEtageAppartement"]
  addrLieu     <- donneesGeneralesNested o ["adresseInstallationNonNormalisee", "lieuDit"]
  addrCP       <- donneesGeneralesNested o ["adresseInstallationNonNormalisee", "codePostal"]
  addrCommune  <- donneesGeneralesNested o ["adresseInstallationNonNormalisee", "commune"]

  -- typage
  typSens      <- donneesGeneralesNestedBool o ["typage", "sensible"]
  typAlimComp  <- donneesGeneralesNestedBool o ["typage", "alimentationComplementaire"]
  typAlimSec   <- donneesGeneralesNestedBool o ["typage", "alimentationSecours"]
  typBornePost <- donneesGeneralesNestedBool o ["typage", "bornePoste"]
  typBorneFix  <- donneesGeneralesNestedBool o ["typage", "borneFixe"]

  -- synthèse contractuelle
  niveauOS     <- syntheseContractuelleNested o ["niveauOuvertureServices", "code"]
  dateModFta   <- syntheseContractuelle o "dateDerniereModificationFormuleTarifaireAcheminement"
  dateAugP     <- syntheseContractuelle o "dateDerniereAugmentationPuissanceSouscrite"
  dateDimP     <- syntheseContractuelle o "dateDerniereDiminutionPuissanceSouscrite"
  dateMesS     <- syntheseContractuelle o "dateDerniereMiseEnServiceSoutirage"
  dateMesI     <- syntheseContractuelle o "dateDerniereMiseEnServiceInjection"
  datePoseLinky <- syntheseContractuelle o "datePremierePoseCompteurLinky"
  telDep       <- syntheseContractuelle o "numeroTelephoneDepannage"
  acColl       <- syntheseContractuelleNested o ["autoConsommationCollective", "libelle"]
  acIndiv      <- syntheseContractuelleNested o ["autoConsommationIndividuelle", "code"]

  -- structure tarifaire
  puissUnit    <- firstSitContractuelleNested o ["structureTarifaire", "puissanceSouscrite", "unite"]
  ftaCode      <- firstSitContractuelleNested o ["structureTarifaire", "formuleTarifaireAcheminement", "code"]
  ftaLib       <- firstSitContractuelleNested o ["structureTarifaire", "formuleTarifaireAcheminement", "libelle"]
  codeTarif    <- firstSitContractuelleNested o ["structureTarifaire", "codeTarifAcheminement"]
  tOffre       <- firstSitContractuelleNested o ["structureTarifaire", "typeOffre"]
  ctxUtil      <- firstSitContractuelleNested o ["structureTarifaire", "contexteUtilisation"]
  forfVal      <- firstSitContractuelleNested o ["structureTarifaire", "forfait", "valeur"]
  forfUnit     <- firstSitContractuelleNested o ["structureTarifaire", "forfait", "unite"]
  calTurpe     <- firstSitContractuelleNested o ["structureTarifaire", "grilleTurpe", "calendrier", "code"]
  grpPM        <- firstSitContractuelleNested o ["structureTarifaire", "groupePeriodeMobile", "code"]
  grpPMD       <- firstSitContractuelleNested o ["structureTarifaire", "groupePeriodeMobileDistributeur", "code"]
  dateDebC     <- firstSitContractuelle o "dateDebut"
  natContrat   <- firstSitContractuelle o "nature"
  typeInj      <- firstSitContractuelleNestedBool o ["typeInjection"]
  refuAmm      <- firstSitContractuelleNestedBool o ["refusPoseAMM"]
  dateRefAmm   <- firstSitContractuelle o "dateRefusPoseAMM"

  -- client final
  catClient    <- clientFinalNested o ["categorie"]
  typeRes      <- clientFinalNested o ["typeResidence"]
  refClient    <- clientFinalNested o ["referenceClient"]
  titCiv       <- clientFinalNested o ["informationsClient", "personnePhysique", "civilite"]
  titNom       <- clientFinalNested o ["informationsClient", "personnePhysique", "nom"]
  titPrenom    <- clientFinalNested o ["informationsClient", "personnePhysique", "prenom"]
  titDenSoc    <- clientFinalNested o ["informationsClient", "personneMorale", "denominationSociale"]
  titNomCom    <- clientFinalNested o ["informationsClient", "personneMorale", "nomCommercial"]
  titSiren     <- clientFinalNested o ["informationsClient", "personneMorale", "numSiren"]
  titSiret     <- clientFinalNested o ["informationsClient", "personneMorale", "numSiret"]
  titSecteur   <- clientFinalNested o ["informationsClient", "personneMorale", "secteur"]
  titNaf       <- clientFinalNested o ["informationsClient", "personneMorale", "activiteNaf"]
  refCont      <- firstSitContractuelleNested o ["informationsContractuelles", "contrat", "referenceContrat"]

  -- situation alimentation
  tensLiv      <- situationAlim o "tensionLivraison"
  puissRacS    <- situationAlimNested o ["alimentationPrincipale", "puissanceRaccordementSoutirage", "valeur"]
  puissRacI    <- situationAlimNested o ["alimentationPrincipale", "puissanceRaccordementInjection", "valeur"]
  puissLimS    <- situationAlimNested o ["alimentationPrincipale", "puissanceLimiteSoutirage", "valeur"]
  tensCont     <- situationAlimNested o ["alimentationPrincipale", "tensionContractuelle", "valeur"]
  modeAlim     <- situationAlimNested o ["alimentationPrincipale", "modeAlimentationApresCompteur"]
  nbFils       <- situationAlimNestedInt o ["alimentationPrincipale", "nbFilsBranchement"]
  zoneQD       <- situationAlimNested o ["alimentationPrincipale", "zoneQualiteDesserte"]
  longAer      <- situationAlimNested o ["alimentationPrincipale", "longueurLiaisonAerienne", "valeur"]
  longSout     <- situationAlimNested o ["alimentationPrincipale", "longueurLiaisonSouterraine", "valeur"]
  prodNb       <- situationAlimNestedInt o ["alimentationPrincipale", "installationClient", "nbMoyensProductionAutonomes"]
  prodPuiss    <- situationAlimNested o ["alimentationPrincipale", "installationClient", "puissanceTotaleMoyensProductionAutonomes", "valeur"]
  coupLoc      <- situationAlimNested o ["coupure", "localisation"]
  coupMotif    <- situationAlimNested o ["coupure", "restriction", "motif"]
  limitPuiss   <- situationAlimNested o ["limiteur", "puissanceLimitee", "valeur"]

  -- dispositif de comptage
  typCompt     <- situationComptageNested o ["dispositifComptage", "typeComptage"]
  modeRel      <- situationComptageNested o ["dispositifComptage", "modeReleve"]
  mediaRel     <- situationComptageNested o ["dispositifComptage", "media"]
  teleop       <- situationComptageNestedBool o ["dispositifComptage", "teleoperable"]
  eligPM       <- situationComptageNestedBool o ["dispositifComptage", "eligiblePeriodeMobile"]
  tensCmpt     <- situationComptageNested o ["dispositifComptage", "tensionComptage"]
  cmptPart     <- situationComptageNested o ["dispositifComptage", "particularite"]
  boitTel      <- situationComptageNestedBool o ["boitierTelereport", "presence"]

  -- compteur [0]
  matricule    <- firstCompteurNested o ["matricule"]
  numSerie     <- firstCompteurNested o ["numeroSerie"]
  ticAct       <- firstCompteurNestedBool o ["ticActivee"]
  ticActab     <- firstCompteurNestedBool o ["ticActivable"]
  ticStd       <- firstCompteurNestedBool o ["ticStandard"]
  periodeLinky <- firstCompteurNested o ["periodeDeploiementLinky"]
  intensNom    <- firstCompteurNested o ["intensiteNominale"]
  puissMax     <- firstCompteurNested o ["puissanceMaximale", "valeur"]
  coefLect     <- firstCompteurNestedDouble o ["coefficientLecture"]
  nbFilsCmpt   <- firstCompteurNestedInt o ["nbFilsCompteur"]
  regPropCmpt  <- firstCompteurNested o ["regimePropriete"]
  cmptAcces    <- firstCompteurNestedBool o ["accessibilite"]
  cmptSit      <- firstCompteurNested o ["situation"]

  -- équipements annexes (dans situationComptage.dispositifComptage)
  disjCal      <- situationComptageNested o ["dispositifComptage", "disjoncteur", "calibre"]
  disjNat      <- situationComptageNested o ["dispositifComptage", "disjoncteur", "nature"]
  disjPoles    <- situationComptageNestedInt o ["dispositifComptage", "disjoncteur", "nombrePoles"]
  disjAcces    <- situationComptageNestedBool o ["dispositifComptage", "disjoncteur", "accessibilite"]
  disjSit      <- situationComptageNested o ["dispositifComptage", "disjoncteur", "situation"]
  disjIntReg   <- situationComptageNested o ["dispositifComptage", "disjoncteur", "intensiteReglage", "valeur"]
  disjRegProp  <- situationComptageNested o ["dispositifComptage", "disjoncteur", "regimePropriete"]
  tcCal        <- situationComptageNested o ["dispositifComptage", "transformateurCourant", "calibre"]
  tcClasse     <- situationComptageNested o ["dispositifComptage", "transformateurCourant", "classePrecision"]
  tcCoup       <- situationComptageNested o ["dispositifComptage", "transformateurCourant", "couplage"]
  tcPos        <- situationComptageNested o ["dispositifComptage", "transformateurCourant", "position"]
  tcRegP       <- situationComptageNested o ["dispositifComptage", "transformateurCourant", "regimePropriete"]
  ttCal        <- situationComptageNested o ["dispositifComptage", "transformateurTension", "calibre"]
  ttClasse     <- situationComptageNested o ["dispositifComptage", "transformateurTension", "classePrecision"]
  ttCoup       <- situationComptageNested o ["dispositifComptage", "transformateurTension", "couplage"]
  pFer         <- situationComptageNestedDouble o ["dispositifComptage", "pertesFer"]
  pJoules      <- situationComptageNestedDouble o ["dispositifComptage", "pertesJoules"]
  pReact       <- situationComptageNestedDouble o ["dispositifComptage", "facteurCorrectifPertesReactives"]
  relNat       <- situationComptageNested o ["dispositifComptage", "relais", "nature"]
  relHC        <- situationComptageNested o ["dispositifComptage", "relais", "plageHeuresCreuses", "libelle"]
  relCmd       <- situationComptageNested o ["dispositifComptage", "relais", "typeCommande"]
  relRegP      <- situationComptageNested o ["dispositifComptage", "relais", "regimePropriete"]

  -- production [0]
  prodFil      <- firstProductionNested o ["filiereProduction"]
  prodTech     <- firstProductionNested o ["technologie"]

  -- listes enfants
  cals         <- parseCalendriers o
  conts        <- parseContinuites o
  quals        <- parseQualites o

  pure InfoTechniqueContractuelle
    { idPrm                        = PrmId pid
    , segment                      = seg
    , etatContractuel              = etat
    , etatAlimentation             = etatAl
    , puissanceSouscrite           = puiss
    , domaineTension               = domTen
    , adresseNumeroNomVoie         = addrNomVoie
    , adresseBatiment              = addrBat
    , adresseEscalierEtage         = addrEsc
    , adresseLieuDit               = addrLieu
    , adresseCodePostal            = addrCP
    , adresseCommune               = addrCommune
    , typageSensible               = typSens
    , typageAlimComplementaire     = typAlimComp
    , typageAlimSecours            = typAlimSec
    , typageBornePoste             = typBornePost
    , typageBorneFixe              = typBorneFix
    , niveauOuvertureServices      = niveauOS
    , dateModifFta                 = dateModFta
    , dateAugmentationPuissance    = dateAugP
    , dateDiminutionPuissance      = dateDimP
    , dateMesSoutirage             = dateMesS
    , dateMesInjection             = dateMesI
    , datePremierePoseLinky        = datePoseLinky
    , telephoneDepannage           = telDep
    , autoConsoCollective          = acColl
    , autoConsoIndividuelle        = acIndiv
    , puissanceSouscriteUnite      = puissUnit
    , formuleTarifaireCode         = ftaCode
    , formuleTarifaireLibelle      = ftaLib
    , codeTarifAcheminement        = codeTarif
    , typeOffre                    = tOffre
    , contexteUtilisation          = ctxUtil
    , forfaitValeur                = forfVal
    , forfaitUnite                 = forfUnit
    , calendrierTurpeCode          = calTurpe
    , groupePeriodeMobile          = grpPM
    , groupePeriodeMobileDistrib   = grpPMD
    , dateDebutContrat             = dateDebC
    , natureContrat                = natContrat
    , typeInjection                = typeInj
    , refusPoseAmm                 = refuAmm
    , dateRefusPoseAmm             = dateRefAmm
    , categorieClient              = catClient
    , typeResidence                = typeRes
    , referenceClient              = refClient
    , titulaireCivilite            = titCiv
    , titulaireNom                 = titNom
    , titulairePrenom              = titPrenom
    , titulaireDenominationSociale = titDenSoc
    , titulaireNomCommercial       = titNomCom
    , titulaireSiren               = titSiren
    , titulaireSiret               = titSiret
    , titulaireSecteur             = titSecteur
    , titulaireActiviteNaf         = titNaf
    , referenceContrat             = refCont
    , tensionLivraison             = tensLiv
    , puissanceRaccordSoutirage    = puissRacS
    , puissanceRaccordInjection    = puissRacI
    , puissanceLimiteSoutirage     = puissLimS
    , tensionContractuelle         = tensCont
    , modeAlimApresCompteur        = modeAlim
    , nbFilsBranchement            = nbFils
    , zoneQualiteDesserte          = zoneQD
    , longueurLiaisonAerienne      = longAer
    , longueurLiaisonSouterraine   = longSout
    , prodAutonomeNb               = prodNb
    , prodAutonomePuissance        = prodPuiss
    , coupureLocalisation          = coupLoc
    , coupureRestrictionMotif      = coupMotif
    , limiteurPuissance            = limitPuiss
    , typeComptage                 = typCompt
    , modeReleve                   = modeRel
    , mediaReleve                  = mediaRel
    , teleoperable                 = teleop
    , eligiblePeriodeMobile        = eligPM
    , tensionComptage              = tensCmpt
    , comptageParticularite        = cmptPart
    , boitierTelereport            = boitTel
    , matriculeCompteur            = matricule
    , numeroSerieCompteur          = numSerie
    , ticActivee                   = ticAct
    , ticActivable                 = ticActab
    , ticStandard                  = ticStd
    , periodeDeploiementLinky      = periodeLinky
    , intensiteNominale            = intensNom
    , puissanceMaxCompteur         = puissMax
    , coefficientLecture           = coefLect
    , nbFilsCompteur               = nbFilsCmpt
    , regimeProprieteCompteur      = regPropCmpt
    , compteurAccessibilite        = cmptAcces
    , compteurSituation            = cmptSit
    , disjoncteurCalibre           = disjCal
    , disjoncteurNature            = disjNat
    , disjoncteurNbPoles           = disjPoles
    , disjoncteurAccessibilite     = disjAcces
    , disjoncteurSituation         = disjSit
    , disjoncteurIntensiteReglage  = disjIntReg
    , disjoncteurRegimePropriete   = disjRegProp
    , tcCalibre                    = tcCal
    , tcClassePrecision            = tcClasse
    , tcCouplage                   = tcCoup
    , tcPosition                   = tcPos
    , tcRegimePropriete            = tcRegP
    , ttCalibre                    = ttCal
    , ttClassePrecision            = ttClasse
    , ttCouplage                   = ttCoup
    , pertesFer                    = pFer
    , pertesJoules                 = pJoules
    , pertesReactives              = pReact
    , relaisNature                 = relNat
    , relaisPlageHc                = relHC
    , relaisTypeCommande           = relCmd
    , relaisRegimePropriete        = relRegP
    , productionFiliere            = prodFil
    , productionTechnologie        = prodTech
    , calendriersFournisseur       = cals
    , continuiteFourniture         = conts
    , qualiteFourniture            = quals
    }

-- ---------------------------------------------------------------------------
-- Helpers de navigation JSON

lookupNested :: Object -> [Text] -> Parser (Maybe Text)
lookupNested _ []     = pure Nothing
lookupNested o [k]    = o .:? Key.fromText k
lookupNested o (k:ks) = do
  mSub <- o .:? Key.fromText k :: Parser (Maybe Object)
  case mSub of
    Just sub -> lookupNested sub ks
    Nothing  -> pure Nothing

lookupNestedBool :: Object -> [Text] -> Parser (Maybe Bool)
lookupNestedBool _ []     = pure Nothing
lookupNestedBool o [k]    = o .:? Key.fromText k
lookupNestedBool o (k:ks) = do
  mSub <- o .:? Key.fromText k :: Parser (Maybe Object)
  case mSub of
    Just sub -> lookupNestedBool sub ks
    Nothing  -> pure Nothing

lookupNestedInt :: Object -> [Text] -> Parser (Maybe Int)
lookupNestedInt _ []     = pure Nothing
lookupNestedInt o [k]    = o .:? Key.fromText k
lookupNestedInt o (k:ks) = do
  mSub <- o .:? Key.fromText k :: Parser (Maybe Object)
  case mSub of
    Just sub -> lookupNestedInt sub ks
    Nothing  -> pure Nothing

lookupNestedDouble :: Object -> [Text] -> Parser (Maybe Double)
lookupNestedDouble _ []     = pure Nothing
lookupNestedDouble o [k]    = o .:? Key.fromText k
lookupNestedDouble o (k:ks) = do
  mSub <- o .:? Key.fromText k :: Parser (Maybe Object)
  case mSub of
    Just sub -> lookupNestedDouble sub ks
    Nothing  -> pure Nothing

firstSitContractuelle :: Object -> Text -> Parser (Maybe Text)
firstSitContractuelle o field = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (\s -> s .:? Key.fromText field) x
    _          -> pure Nothing

firstSitContractuelleNested :: Object -> [Text] -> Parser (Maybe Text)
firstSitContractuelleNested o path = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (`lookupNested` path) x
    _          -> pure Nothing

firstSitContractuelleNestedBool :: Object -> [Text] -> Parser (Maybe Bool)
firstSitContractuelleNestedBool o path = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (`lookupNestedBool` path) x
    _          -> pure Nothing

situationAlim :: Object -> Text -> Parser (Maybe Text)
situationAlim o field = do
  mSit <- o .:? "situationAlimentation" :: Parser (Maybe Object)
  case mSit of
    Just sit -> sit .:? Key.fromText field
    Nothing  -> pure Nothing

situationAlimNested :: Object -> [Text] -> Parser (Maybe Text)
situationAlimNested o path = do
  mSit <- o .:? "situationAlimentation" :: Parser (Maybe Object)
  case mSit of
    Just sit -> lookupNested sit path
    Nothing  -> pure Nothing

situationAlimNestedInt :: Object -> [Text] -> Parser (Maybe Int)
situationAlimNestedInt o path = do
  mSit <- o .:? "situationAlimentation" :: Parser (Maybe Object)
  case mSit of
    Just sit -> lookupNestedInt sit path
    Nothing  -> pure Nothing

donneesGeneralesNested :: Object -> [Text] -> Parser (Maybe Text)
donneesGeneralesNested o path = do
  mDg <- o .:? "donneesGenerales" :: Parser (Maybe Object)
  case mDg of
    Just dg -> lookupNested dg path
    Nothing -> pure Nothing

donneesGeneralesNestedBool :: Object -> [Text] -> Parser (Maybe Bool)
donneesGeneralesNestedBool o path = do
  mDg <- o .:? "donneesGenerales" :: Parser (Maybe Object)
  case mDg of
    Just dg -> lookupNestedBool dg path
    Nothing -> pure Nothing

syntheseContractuelle :: Object -> Text -> Parser (Maybe Text)
syntheseContractuelle o field = do
  mSc <- o .:? "syntheseContractuelle" :: Parser (Maybe Object)
  case mSc of
    Just sc -> sc .:? Key.fromText field
    Nothing -> pure Nothing

syntheseContractuelleNested :: Object -> [Text] -> Parser (Maybe Text)
syntheseContractuelleNested o path = do
  mSc <- o .:? "syntheseContractuelle" :: Parser (Maybe Object)
  case mSc of
    Just sc -> lookupNested sc path
    Nothing -> pure Nothing

situationComptageNested :: Object -> [Text] -> Parser (Maybe Text)
situationComptageNested o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Just sc -> lookupNested sc path
    Nothing -> pure Nothing

situationComptageNestedBool :: Object -> [Text] -> Parser (Maybe Bool)
situationComptageNestedBool o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Just sc -> lookupNestedBool sc path
    Nothing -> pure Nothing

situationComptageNestedInt :: Object -> [Text] -> Parser (Maybe Int)
situationComptageNestedInt o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Just sc -> lookupNestedInt sc path
    Nothing -> pure Nothing

situationComptageNestedDouble :: Object -> [Text] -> Parser (Maybe Double)
situationComptageNestedDouble o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Just sc -> lookupNestedDouble sc path
    Nothing -> pure Nothing

clientFinalNested :: Object -> [Text] -> Parser (Maybe Text)
clientFinalNested o path = do
  sits <- o .:? "situationsContractuelles" :: Parser (Maybe [Value])
  case sits of
    Just (x:_) -> withObject "SitContractuelle" (\s -> do
      mCf <- s .:? "clientFinal" :: Parser (Maybe Object)
      case mCf of
        Just cf -> lookupNested cf path
        Nothing -> pure Nothing) x
    _ -> pure Nothing

firstCompteurNested :: Object -> [Text] -> Parser (Maybe Text)
firstCompteurNested o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Nothing -> pure Nothing
    Just sc -> do
      mCompteurs <- sc .:? "compteurs" :: Parser (Maybe Value)
      case mCompteurs of
        Just (Array arr) | not (V.null arr) ->
          withObject "Compteur" (`lookupNested` path) (V.head arr)
        _ -> pure Nothing

firstCompteurNestedBool :: Object -> [Text] -> Parser (Maybe Bool)
firstCompteurNestedBool o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Nothing -> pure Nothing
    Just sc -> do
      mCompteurs <- sc .:? "compteurs" :: Parser (Maybe Value)
      case mCompteurs of
        Just (Array arr) | not (V.null arr) ->
          withObject "Compteur" (`lookupNestedBool` path) (V.head arr)
        _ -> pure Nothing

firstCompteurNestedInt :: Object -> [Text] -> Parser (Maybe Int)
firstCompteurNestedInt o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Nothing -> pure Nothing
    Just sc -> do
      mCompteurs <- sc .:? "compteurs" :: Parser (Maybe Value)
      case mCompteurs of
        Just (Array arr) | not (V.null arr) ->
          withObject "Compteur" (`lookupNestedInt` path) (V.head arr)
        _ -> pure Nothing

firstCompteurNestedDouble :: Object -> [Text] -> Parser (Maybe Double)
firstCompteurNestedDouble o path = do
  mSc <- o .:? "situationComptage" :: Parser (Maybe Object)
  case mSc of
    Nothing -> pure Nothing
    Just sc -> do
      mCompteurs <- sc .:? "compteurs" :: Parser (Maybe Value)
      case mCompteurs of
        Just (Array arr) | not (V.null arr) ->
          withObject "Compteur" (`lookupNestedDouble` path) (V.head arr)
        _ -> pure Nothing

firstProductionNested :: Object -> [Text] -> Parser (Maybe Text)
firstProductionNested o path = do
  mArr <- o .:? "installationsProduction" :: Parser (Maybe Value)
  case mArr of
    Just (Array arr) | not (V.null arr) ->
      withObject "Production" (`lookupNested` path) (V.head arr)
    _ -> pure Nothing

parseCalendriers :: Object -> Parser [CalendrierFournisseur]
parseCalendriers o = do
  mOpts <- o .:? "optionsContractuelles" :: Parser (Maybe Object)
  case mOpts of
    Nothing -> pure []
    Just opts -> do
      mArr <- opts .:? "calendriersFournisseur" :: Parser (Maybe [Value])
      case mArr of
        Nothing   -> pure []
        Just vals -> mapM parseCalendrier vals
  where
    parseCalendrier = withObject "CalendrierFournisseur" $ \c -> do
      code <- c .: "code"
      pm   <- c .:? "periodeMobileAutorisee"
      prof <- c .:? "profilable"
      pure (CalendrierFournisseur code pm prof)

parseContinuites :: Object -> Parser [ContinuiteFourniture]
parseContinuites o = do
  mOpts <- o .:? "optionsContractuelles" :: Parser (Maybe Object)
  case mOpts of
    Nothing -> pure []
    Just opts -> do
      mArr <- opts .:? "bilansContinuiteFourniture" :: Parser (Maybe [Value])
      case mArr of
        Nothing   -> pure []
        Just vals -> mapM parseContinuite vals
  where
    parseContinuite = withObject "ContinuiteFourniture" $ \c ->
      ContinuiteFourniture
        <$> c .:? "idFonctionnel"
        <*> c .:? "type"
        <*> c .:? "periodicite"
        <*> c .:? "dateReference"
        <*> c .:? "moisReference"
        <*> c .:? "nombreCoupuresBreves"
        <*> c .:? "nombreCoupuresLongues"
        <*> c .:? "nombreTotalCoupures"
        <*> c .:? "typeCoupures"

parseQualites :: Object -> Parser [QualiteFourniture]
parseQualites o = do
  mOpts <- o .:? "optionsContractuelles" :: Parser (Maybe Object)
  case mOpts of
    Nothing -> pure []
    Just opts -> do
      mArr <- opts .:? "bilansQualiteFourniture" :: Parser (Maybe [Value])
      case mArr of
        Nothing   -> pure []
        Just vals -> mapM parseQualite vals
  where
    parseQualite = withObject "QualiteFourniture" $ \q ->
      QualiteFourniture
        <$> q .:? "idFonctionnel"
        <*> q .:? "periodicite"
        <*> q .:? "dateReference"
        <*> q .:? "nombreCreux"
        <*> q .:? "profondeurCreux"
        <*> (q .:? "dureeCreux" >>= \case
               Nothing -> pure Nothing
               Just dc -> withObject "dureeCreux" (.:? "valeur") dc)
        <*> (q .:? "dureeCreux" >>= \case
               Nothing -> pure Nothing
               Just dc -> withObject "dureeCreux" (.:? "unite") dc)

-- | Parse un fichier C68 dont la racine JSON est un tableau.
parseFluxC68 :: Value -> Either String [InfoTechniqueContractuelle]
parseFluxC68 v = case fromJSON v of
  Success items -> Right items
  Error msg     -> Left $ "Erreur parsing C68: " ++ msg
