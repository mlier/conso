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
import qualified Data.ByteString.Lazy          as LBS
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
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
import           Conso.Fr.SiteDB.Registry               (openRegistry, lookupOrCreateByPce)
import           Conso.Fr.Gaz.SiteDB.Storage.Connection (openSiteDbGaz)
import           Conso.Fr.Gaz.SiteDB.Storage.Insert
import           Conso.Fr.Gaz.SiteDB.Storage.Query
    (derniereInfosContractuelles, derniereInfosTechniques)
import           Conso.Fr.Gaz.SiteDB.Types


-- ---------------------------------------------------------------------------
-- Helpers de conversion

encodeText :: A.ToJSON a => a -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . A.encode

adictErrorToText :: AdictError -> Text
adictErrorToText (HttpError code body)      = "HTTP " <> T.pack (show code) <> ": " <> body
adictErrorToText (ParseError msg)           = "Parse error: " <> msg
adictErrorToText (AuthError msg)            = "Auth error: " <> msg
adictErrorToText (NetworkError msg)         = "Network error: " <> msg
adictErrorToText (FunctionalError code msg) = "Erreur métier " <> code <> ": " <> msg

inferPeriode :: Maybe Text -> PeriodeGaz
inferPeriode (Just v) | T.length v == 10 = PJournalier
inferPeriode _                            = PMensuel

toGazConso :: TypeDonnee -> ConsoRestit -> Maybe GazConso
toGazConso td cr = do
  let conso = cr_consommation cr
      per   = cr_periode cr
  d1 <- (conso >>= date_debut_consommation) <> (per >>= date_debut)
  d2 <- (conso >>= date_fin_consommation)   <> (per >>= date_fin)
  pure GazConso
    { gcDateDebut       = d1
    , gcDateFin         = d2
    , gcPeriode         = inferPeriode (per >>= valeur)
    , gcTypeDonnee      = td
    , gcEnergie         = conso >>= energie
    , gcVolumeBrut      = conso >>= volume_brut
    , gcVolumeConverti  = conso >>= volume_converti
    , gcCoeffConversion = (conso >>= coeff_calcul) >>= coeff_conversion
    , gcCoeffPta        = (conso >>= coeff_calcul) >>= coeff_pta
    , gcRawJson         = encodeText cr
    }

toGazInjection :: TypeDonnee -> InjectionRestit -> Maybe GazInjection
toGazInjection td ir = do
  let inj = ir_injection ir
      per = ir_periode ir
  d1 <- (inj >>= date_debut_injection) <> (per >>= date_debut)
  d2 <- (inj >>= date_fin_injection)   <> (per >>= date_fin)
  pure GazInjection
    { giDateDebut      = d1
    , giDateFin        = d2
    , giPeriode        = inferPeriode (per >>= valeur)
    , giTypeDonnee     = td
    , giEnergie        = inj >>= inj_energie
    , giVolumeBrut     = inj >>= inj_volume_brut
    , giVolumeConverti = inj >>= inj_volume_converti
    , giRawJson        = encodeText ir
    }

toGazInfosContractuelles :: RetourDonneesContractuelles -> GazInfosContractuelles
toGazInfosContractuelles r = GazInfosContractuelles
  { icDateDebut     = rdc_donnees r >>= dc_date_mes
  , icDateFin       = Nothing
  , icSegmentClient = Nothing
  , icNumCompteur   = Nothing
  , icTarif         = rdc_donnees r >>= dc_tarif_acheminement
  , icRawJson       = encodeText r
  }

toGazInfosTechniques :: RetourDonneesTechniques -> GazInfosTechniques
toGazInfosTechniques r = GazInfosTechniques
  { itTypeCompteur = Nothing
  , itPression     = Nothing
  , itDateReleve   = Nothing
  , itEtatCompteur = Nothing
  , itRawJson      = encodeText r
  }

-- | Compare les champs métier uniquement (exclut raw_json qui peut différer
-- même pour des données identiques selon la sérialisation API).
memeChampsBusiness :: GazInfosContractuelles -> GazInfosContractuelles -> Bool
memeChampsBusiness a b =
  icDateDebut a == icDateDebut b &&
  icDateFin   a == icDateFin   b &&
  icSegmentClient a == icSegmentClient b &&
  icNumCompteur   a == icNumCompteur   b &&
  icTarif a == icTarif b

memeChampsTech :: GazInfosTechniques -> GazInfosTechniques -> Bool
memeChampsTech a b =
  itTypeCompteur a == itTypeCompteur b &&
  itPression     a == itPression     b &&
  itDateReleve   a == itDateReleve   b &&
  itEtatCompteur a == itEtatCompteur b


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
      let rows = mapMaybe (toGazConso TDPubliee) consos
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
      let rows = mapMaybe (toGazConso TDInformative) consos
      ingId <- logGazIngestion conn "donnees_consos_informatives"
                 (Just dateDebut) (Just dateFin) Nothing (Just "INFORMATIVE") now (length rows)
      insertGazConsos conn ingId rows
      return $ Right (length rows)

-- | Ingère les injections publiées pour un PCE sur une plage de dates.
ingererInjections
  :: AdictSession -> Connection -> Pce -> Text -> Text -> IO (Either Text Int)
ingererInjections session conn pce dateDebut dateFin = do
  now <- getCurrentTime
  consulterInjectionsPubliees session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err  -> return $ Left (adictErrorToText err)
    Right injs -> do
      let rows = mapMaybe (toGazInjection TDPubliee) injs
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
        Just derniere | memeChampsBusiness nouvelles derniere ->
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
        Just derniere | memeChampsTech nouvelles derniere ->
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
  reg    <- openRegistry configDir
  siteId <- lookupOrCreateByPce reg pce
  conn   <- openSiteDbGaz siteDbDir siteId
  rPub   <- ingererConsosPubliees      session conn pce dateDebut dateFin
  rInfo  <- ingererConsosInfos         session conn pce dateDebut dateFin
  rInj   <- ingererInjections          session conn pce dateDebut dateFin
  rCont  <- ingererInfosContractuelles session conn pce
  rTech  <- ingererInfosTechniques     session conn pce
  return $ AdictIngestReport pce rPub rInfo rInj rCont rTech
