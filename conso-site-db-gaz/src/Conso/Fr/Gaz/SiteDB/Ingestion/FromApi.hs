{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Conso.Fr.Gaz.SiteDB.Ingestion.FromApi
  ( -- * Types
    AdictIngestReport(..)
  , ChangementInfosContract(..)
  , ChangementInfosTech(..)
    -- * Ingestion par endpoint
  , ingererConsosPubliees
  , ingererConsosInfos
  , ingererInjections
  , ingererInfosContractuelles
  , ingererInfosTechniques
    -- * Ingestion complète (wrapper commode)
  , ingestFromAdict
  ) where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Key                as Key
import qualified Data.Aeson.KeyMap             as KM
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (getCurrentTime)

import           Database.SQLite.Simple        (Connection)

import           Conso.Fr.Gaz.Adict.Adict      (AdictError(..), AdictSession)
import           Conso.Fr.Gaz.Adict.ConsosInfos (consulterConsosInfos)
import           Conso.Fr.Gaz.Adict.ConsosPubliees
    (PeriodeParam(..), consulterConsosPubliees)
import           Conso.Fr.Gaz.Adict.DonneesContractuelles
    (consulterDonneesContractuelles)
import           Conso.Fr.Gaz.Adict.DonneesTechniques
    (consulterDonneesTechniques)
import           Conso.Fr.Gaz.Adict.InjectionsPubliees
    (consulterInjectionsPubliees)
import           Conso.Fr.Gaz.Adict.Types

import           Conso.Fr.SiteDB.Types                  (Pce(..))
import           Conso.Fr.SiteDB.Registry               (openRegistry, lookupByPce)
import           Conso.Fr.Gaz.SiteDB.Storage.Connection (openSiteDbGaz)
import           Conso.Fr.Gaz.SiteDB.Storage.Insert
import           Conso.Fr.Gaz.SiteDB.Storage.Query
    (derniereInfosContractuelles, derniereInfosTechniques)
import           Conso.Fr.Gaz.SiteDB.Types


-- ---------------------------------------------------------------------------
-- Helpers de conversion

adictErrorToText :: AdictError -> Text
adictErrorToText (HttpError code body)      = "HTTP " <> T.pack (show code) <> ": " <> body
adictErrorToText (ParseError msg)           = "Parse error: " <> msg
adictErrorToText (AuthError msg)            = "Auth error: " <> msg
adictErrorToText (NetworkError msg)         = "Network error: " <> msg
adictErrorToText (FunctionalError code msg) = "Erreur métier " <> code <> ": " <> msg

toGazConso :: ConsoRestit -> Maybe GazConso
toGazConso cr = do
  let conso = cr_consommation cr
      rDeb  = cr_releve_debut cr
      rFin  = cr_releve_fin cr
      coeff = conso >>= coeff_calcul
  deb <- rDeb >>= rd_date_releve
  fin <- rFin >>= rf_date_releve
  pure GazConso
    { gcEnergie            = conso >>= energie
    , gcVolumeBrut         = conso >>= volume_brut
    , gcVolumeConverti     = conso >>= volume_converti
    , gcConversion         = coeff >>= coeff_conversion
    , gcPta                = coeff >>= coeff_pta
    , gcPcs                = coeff >>= valeur_pcs
    , gcFlagRetourZero     = conso >>= flag_retour_zero
    , gcTypeQualif         = conso >>= type_qualif_conso
    , gcSensFlux           = conso >>= sens_flux_gaz
    , gcStatutConso        = conso >>= statut_conso
    , gcTypeConso          = conso >>= type_conso
    , gcJourneeGaziere     = conso >>= journee_gaziere
    , gcDebut              = deb
    , gcDebutRaison        = rDeb >>= rd_raison_releve
    , gcDebutLibelleRaison = rDeb >>= rd_libelle_raison_releve
    , gcDebutQualite       = rDeb >>= rd_qualite_releve
    , gcDebutStatut        = rDeb >>= rd_statut_releve
    , gcDebutIndexBrut     = (rDeb >>= rd_index_brut_debut)     >>= valeur_index
    , gcDebutIndexConverti = (rDeb >>= rd_index_converti_debut) >>= valeur_index
    , gcFin                = fin
    , gcFinRaison          = rFin >>= rf_raison_releve
    , gcFinLibelleRaison   = rFin >>= rf_libelle_raison_releve
    , gcFinQualite         = rFin >>= rf_qualite_releve
    , gcFinStatut          = rFin >>= rf_statut_releve
    , gcFinIndexBrut       = (rFin >>= rf_index_brut_fin)     >>= valeur_index
    , gcFinIndexConverti   = (rFin >>= rf_index_converti_fin) >>= valeur_index
    }

toGazInjection :: InjectionRestit -> Maybe GazInjection
toGazInjection ir = do
  let inj   = ir_injection ir
      rDeb  = ir_releve_debut ir
      rFin  = ir_releve_fin ir
      coeff = inj >>= inj_coeff_calcul
  deb <- rDeb >>= rd_date_releve
  fin <- rFin >>= rf_date_releve
  pure GazInjection
    { giEnergie            = inj >>= inj_energie
    , giVolumeBrut         = inj >>= inj_volume_brut
    , giVolumeConverti     = inj >>= inj_volume_converti
    , giConversion         = coeff >>= coeff_conversion
    , giPta                = coeff >>= coeff_pta
    , giPcs                = coeff >>= valeur_pcs
    , giFlagRetourZero     = inj >>= inj_flag_retour_zero
    , giTypeQualif         = inj >>= type_qualif_injection
    , giSensFlux           = inj >>= inj_sens_flux_gaz
    , giStatut             = inj >>= statut_injection
    , giTypeInjection      = inj >>= type_injection
    , giJourneeGaziere     = inj >>= inj_journee_gaziere
    , giDebut              = deb
    , giDebutRaison        = rDeb >>= rd_raison_releve
    , giDebutLibelleRaison = rDeb >>= rd_libelle_raison_releve
    , giDebutQualite       = rDeb >>= rd_qualite_releve
    , giDebutStatut        = rDeb >>= rd_statut_releve
    , giDebutIndexBrut     = (rDeb >>= rd_index_brut_debut)     >>= valeur_index
    , giDebutIndexConverti = (rDeb >>= rd_index_converti_debut) >>= valeur_index
    , giFin                = fin
    , giFinRaison          = rFin >>= rf_raison_releve
    , giFinLibelleRaison   = rFin >>= rf_libelle_raison_releve
    , giFinQualite         = rFin >>= rf_qualite_releve
    , giFinStatut          = rFin >>= rf_statut_releve
    , giFinIndexBrut       = (rFin >>= rf_index_brut_fin)     >>= valeur_index
    , giFinIndexConverti   = (rFin >>= rf_index_converti_fin) >>= valeur_index
    }

-- | Extrait un champ texte depuis un sous-objet JSON (Maybe Value).
getField :: Text -> Maybe A.Value -> Maybe Text
getField key (Just (A.Object m)) = case KM.lookup (Key.fromText key) m of
  Just (A.String t) -> Just t
  Just (A.Number n) -> Just (T.pack (show n))
  _                 -> Nothing
getField _ _ = Nothing

toGazInfosContractuelles :: RetourDonneesContractuelles -> GazInfosContractuelles
toGazInfosContractuelles r =
  let dc = rdc_donnees r
  in GazInfosContractuelles
    { icDateMes                   = dc >>= dc_date_mes
    , icTarifAcheminement         = dc >>= dc_tarif_acheminement
    , icDatePublication           = dc >>= dc_date_publication
    , icConsoJournalierePlafond   = dc >>= dc_consommation_journaliere_plafond
    , icCarActuelle               = getField "car_actuelle"               (dc >>= dc_car)
    , icCarFuture                 = getField "car_future"                 (dc >>= dc_car)
    , icCja                       = getField "cja"                        (dc >>= dc_cja)
    , icCjaJournaliere            = getField "cja_journaliere"            (dc >>= dc_cja)
    , icCjaMensuelle              = getField "cja_mensuelle"              (dc >>= dc_cja)
    , icProfilTypeActuel          = getField "profil_type_actuel"         (dc >>= dc_profil)
    , icProfilTypeFutur           = getField "profil_type_futur"          (dc >>= dc_profil)
    , icDateDebutProfilTypeActuel = getField "date_debut_profil_type_actuel" (dc >>= dc_profil)
    , icDateFinProfilTypeActuel   = getField "date_fin_profil_type_actuel"   (dc >>= dc_profil)
    , icModulationAssiette        = getField "assiette"                   (dc >>= dc_modulation)
    , icModulationN1              = getField "modulation_n_1"             (dc >>= dc_modulation)
    , icModulationN2              = getField "modulation_n_2"             (dc >>= dc_modulation)
    , icModulationN3              = getField "modulation_n_3"             (dc >>= dc_modulation)
    }

toGazInfosTechniques :: RetourDonneesTechniques -> GazInfosTechniques
toGazInfosTechniques r =
  let dt     = rdt_donnees r
      sit    = dt >>= dt_situation_compteur
      carac  = dt >>= dt_caracteristiques_compteur
      pitd   = dt >>= dt_pitd
      regime = dt >>= dt_regime_propriete
  in GazInfosTechniques
    { itNumeroRue                    = sit >>= numero_rue
    , itNomRue                       = sit >>= nom_rue
    , itComplementAdresse            = sit >>= complement_adresse
    , itCodePostal                   = sit >>= scd_code_postal
    , itCommune                      = sit >>= commune
    , itClientSensibleMig            = getField "client_sensible_mig"           carac
    , itCodeCalibre                  = getField "code_calibre"                  carac
    , itCodeDebit                    = getField "code_debit"                    carac
    , itCodeDebitNormalise           = getField "code_debit_normalise"          carac
    , itFrequence                    = getField "frequence"                     carac
    , itMatriculeCompteur            = getField "matricule_compteur"            carac
    , itPressionLivraison            = getField "pression_livraison"            carac
    , itIdentifiantPitd              = pitd >>= identifiant_pitd
    , itLibellePitd                  = pitd >>= libelle_pitd
    , itRegimeProprieteCompteur      = getField "regime_propriete_compteur"      regime
    , itRegimeProprieteConvertisseur = getField "regime_propriete_convertisseur" regime
    , itRegimeProprieteEnregistreur  = getField "regime_propriete_enregistreur"  regime
    , itRegimeProprietePoste         = getField "regime_propriete_poste"         regime
    }


-- ---------------------------------------------------------------------------
-- Types de résultat

data ChangementInfosContract
  = ContractuellesPasDeChangement
  | ContractuellesNouvellesInfos GazInfosContractuelles
  deriving (Show)

data ChangementInfosTech
  = TechniquesPasDeChangement
  | TechniquesNouvellesInfos GazInfosTechniques
  deriving (Show)

data AdictIngestReport = AdictIngestReport
  { airPce            :: Pce
  , airConsosPubliees :: Either Text Int
  , airConsosInfos    :: Either Text Int
  , airInjections     :: Either Text Int
  , airInfosContract  :: Either Text ChangementInfosContract
  , airInfosTech      :: Either Text ChangementInfosTech
  } deriving (Show)


-- ---------------------------------------------------------------------------
-- Ingestion par endpoint

pceText :: Pce -> Text
pceText (Pce t) = t

-- | Ingère les consommations publiées pour un PCE sur une plage de dates.
ingererConsosPubliees
  :: AdictSession -> Connection -> Pce -> Text -> Text -> IO (Either Text Int)
ingererConsosPubliees session conn pce dateDebut dateFin = do
  now <- getCurrentTime
  consulterConsosPubliees session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right consos -> do
      let rows = mapMaybe toGazConso consos
      ingId <- logGazIngestion conn "donnees_consos_publiees"
                 (Just dateDebut) (Just dateFin) Nothing (Just "PUBLIEE") now (length rows)
      insertGazConsos conn ingId rows
      return $ Right (length rows)

-- | Ingère les consommations informatives pour un PCE sur une plage de dates.
ingererConsosInfos
  :: AdictSession -> Connection -> Pce -> Text -> Text -> IO (Either Text Int)
ingererConsosInfos session conn pce dateDebut dateFin = do
  now <- getCurrentTime
  consulterConsosInfos session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right consos -> do
      let rows = mapMaybe toGazConso consos
      ingId <- logGazIngestion conn "donnees_consos_informatives"
                 (Just dateDebut) (Just dateFin) Nothing (Just "INFORMATIVE") now (length rows)
      insertGazConsosInformatives conn ingId rows
      return $ Right (length rows)

-- | Ingère les injections publiées pour un PCE sur une plage de dates.
ingererInjections
  :: AdictSession -> Connection -> Pce -> Text -> Text -> IO (Either Text Int)
ingererInjections session conn pce dateDebut dateFin = do
  now <- getCurrentTime
  consulterInjectionsPubliees session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err  -> return $ Left (adictErrorToText err)
    Right injs -> do
      let rows = mapMaybe toGazInjection injs
      ingId <- logGazIngestion conn "donnees_injections_publiees"
                 (Just dateDebut) (Just dateFin) Nothing (Just "PUBLIEE") now (length rows)
      insertGazInjections conn ingId rows
      return $ Right (length rows)

-- | Ingère les informations contractuelles — stocke uniquement si les champs
-- métier ont changé par rapport à la dernière valeur connue.
ingererInfosContractuelles
  :: AdictSession -> Connection -> Pce -> IO (Either Text ChangementInfosContract)
ingererInfosContractuelles session conn pce = do
  now <- getCurrentTime
  consulterDonneesContractuelles session (pceText pce) [] >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right retour -> do
      let nouvelles = toGazInfosContractuelles retour
      mDerniere <- derniereInfosContractuelles conn
      case mDerniere of
        Just derniere | nouvelles == derniere ->
          return $ Right ContractuellesPasDeChangement
        _ -> do
          ingId <- logGazIngestion conn "donnees_contractuelles"
                     Nothing Nothing Nothing Nothing now 1
          insertGazInfosContractuelles conn ingId now nouvelles
          return $ Right (ContractuellesNouvellesInfos nouvelles)

-- | Ingère les informations techniques — stocke uniquement si les champs
-- métier ont changé par rapport à la dernière valeur connue.
ingererInfosTechniques
  :: AdictSession -> Connection -> Pce -> IO (Either Text ChangementInfosTech)
ingererInfosTechniques session conn pce = do
  now <- getCurrentTime
  consulterDonneesTechniques session (pceText pce) >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right retour -> do
      let nouvelles = toGazInfosTechniques retour
      mDerniere <- derniereInfosTechniques conn
      case mDerniere of
        Just derniere | nouvelles == derniere ->
          return $ Right TechniquesPasDeChangement
        _ -> do
          ingId <- logGazIngestion conn "donnees_techniques"
                     Nothing Nothing Nothing Nothing now 1
          insertGazInfosTechniques conn ingId now nouvelles
          return $ Right (TechniquesNouvellesInfos nouvelles)


-- ---------------------------------------------------------------------------
-- Wrapper : ingestion complète sur une plage unique (compatibilité)

-- | Ingère toutes les données ADICT pour un PCE sur une période.
-- Ouvre/crée le site via le registre, puis insère chaque type de donnée.
-- Utilise la même plage de dates pour tous les endpoints temporels.
ingestFromAdict
  :: AdictSession
  -> FilePath -- ^ Répertoire de configuration (contient registry.db)
  -> FilePath -- ^ Répertoire des bases SQLite site
  -> Pce
  -> Text     -- ^ Date de début (YYYY-MM-DD)
  -> Text     -- ^ Date de fin (YYYY-MM-DD)
  -> IO AdictIngestReport
ingestFromAdict session configDir siteDbDir pce dateDebut dateFin = do
  reg     <- openRegistry configDir
  mSiteId <- lookupByPce reg pce
  siteId  <- case mSiteId of
    Nothing       -> let Pce pId = pce
                     in fail $ "PCE non inscrit dans le registre : " <> T.unpack pId
    Just sid      -> return sid
  conn   <- openSiteDbGaz siteDbDir siteId
  rPub   <- ingererConsosPubliees      session conn pce dateDebut dateFin
  rInfo  <- ingererConsosInfos         session conn pce dateDebut dateFin
  rInj   <- ingererInjections          session conn pce dateDebut dateFin
  rCont  <- ingererInfosContractuelles session conn pce
  rTech  <- ingererInfosTechniques     session conn pce
  return $ AdictIngestReport pce rPub rInfo rInj rCont rTech
