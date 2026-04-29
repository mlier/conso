{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-|
Module      : Conso.Fr.Gaz.SiteDB.Ingestion.FromApi
Description : Ingestion des données gaz depuis l'API GRDF ADICT

Orchestre l'appel aux endpoints ADICT, l'ouverture de la base site et
l'insertion des données gaz dans les tables @gaz_*@.

Ce module dépend directement de @conso-gaz-adict@ : il appelle les webservices
ADICT et convertit les réponses en types SQLite avant insertion.

Usage typique :

@
session <- initSession True False     -- production
report  <- ingestFromAdict session "~\/.conso" "~\/.conso\/sites"
             (Pce "12345678901234") "2024-01-01" "2024-12-31"
@
-}
module Conso.Fr.Gaz.SiteDB.Ingestion.FromApi
  ( AdictIngestReport(..)
  , ingestFromAdict
  ) where

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as LBS
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Time                     (getCurrentTime)

import           Conso.Fr.Gaz.Adict.Adict
    ( AdictError(..), AdictSession )
import           Conso.Fr.Gaz.Adict.ConsosInfos
    ( consulterConsosInfos )
import           Conso.Fr.Gaz.Adict.ConsosPubliees
    ( PeriodeParam(..), consulterConsosPubliees )
import           Conso.Fr.Gaz.Adict.DonneesContractuelles
    ( consulterDonneesContractuelles )
import           Conso.Fr.Gaz.Adict.DonneesTechniques
    ( consulterDonneesTechniques )
import           Conso.Fr.Gaz.Adict.InjectionsPubliees
    ( consulterInjectionsPubliees )
import           Conso.Fr.Gaz.Adict.Types

import           Conso.Fr.SiteDB.Types                        (Pce(..))
import           Conso.Fr.SiteDB.Registry                     (openRegistry, lookupOrCreateByPce)
import           Conso.Fr.Gaz.SiteDB.Storage.Connection       (openSiteDbGaz)
import           Conso.Fr.Gaz.SiteDB.Types
import           Conso.Fr.Gaz.SiteDB.Storage.Insert


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

-- | Infère la granularité depuis la valeur de période ADICT.
-- Une valeur de 10 caractères (\"YYYY-MM-DD\") correspond à une journée gazière.
inferPeriode :: Maybe Text -> PeriodeGaz
inferPeriode (Just v) | T.length v == 10 = PJournalier
inferPeriode _                            = PMensuel

-- | Convertit un 'ConsoRestit' ADICT en 'GazConso' SQLite.
-- Retourne 'Nothing' si les dates sont absentes (ligne inutilisable).
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

-- | Convertit un 'InjectionRestit' ADICT en 'GazInjection' SQLite.
-- Retourne 'Nothing' si les dates sont absentes.
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

-- | Convertit un 'RetourDonneesContractuelles' en 'GazInfosContractuelles'.
toGazInfosContractuelles :: RetourDonneesContractuelles -> GazInfosContractuelles
toGazInfosContractuelles r = GazInfosContractuelles
  { icDateDebut     = rdc_donnees r >>= dc_date_mes
  , icDateFin       = Nothing
  , icSegmentClient = Nothing
  , icNumCompteur   = Nothing
  , icTarif         = rdc_donnees r >>= dc_tarif_acheminement
  , icRawJson       = encodeText r
  }

-- | Convertit un 'RetourDonneesTechniques' en 'GazInfosTechniques'.
-- Les caractéristiques détaillées sont conservées dans le JSON brut.
toGazInfosTechniques :: RetourDonneesTechniques -> GazInfosTechniques
toGazInfosTechniques r = GazInfosTechniques
  { itTypeCompteur = Nothing
  , itPression     = Nothing
  , itDateReleve   = Nothing
  , itEtatCompteur = Nothing
  , itRawJson      = encodeText r
  }


-- ---------------------------------------------------------------------------
-- Rapport

-- | Rapport d'une opération d'ingestion ADICT.
data AdictIngestReport = AdictIngestReport
  { airPce            :: Pce
  , airConsosPubliees :: Either Text Int   -- ^ nb insérés ou erreur
  , airConsosInfos    :: Either Text Int
  , airInjections     :: Either Text Int
  , airInfosContract  :: Either Text Bool  -- ^ True si inséré
  , airInfosTech      :: Either Text Bool
  } deriving (Show)


-- ---------------------------------------------------------------------------
-- Ingestion

-- | Ingère toutes les données ADICT pour un PCE sur une période.
-- Ouvre/crée le site via le registre, puis insère chaque type de donnée.
ingestFromAdict
  :: AdictSession
  -> FilePath -- ^ Répertoire de configuration (contient @registry.db@)
  -> FilePath -- ^ Répertoire des bases SQLite site
  -> Pce      -- ^ PCE à ingérer
  -> Text     -- ^ Date de début (YYYY-MM-DD)
  -> Text     -- ^ Date de fin (YYYY-MM-DD)
  -> IO AdictIngestReport
ingestFromAdict session configDir siteDbDir pce dateDebut dateFin = do
  reg    <- openRegistry configDir
  siteId <- lookupOrCreateByPce reg pce
  conn   <- openSiteDbGaz siteDbDir siteId
  now    <- getCurrentTime
  let pceText (Pce t) = t

  -- Consommations publiées
  rPub <- consulterConsosPubliees session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right consos -> do
      let rows = mapMaybe (toGazConso TDPubliee) consos
      ingId <- logGazIngestion conn "donnees_consos_publiees"
                 (Just dateDebut) (Just dateFin) Nothing (Just "PUBLIEE") now (length rows)
      insertGazConsos conn ingId rows
      return $ Right (length rows)

  -- Consommations informatives
  rInfo <- consulterConsosInfos session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right consos -> do
      let rows = mapMaybe (toGazConso TDInformative) consos
      ingId <- logGazIngestion conn "donnees_consos_informatives"
                 (Just dateDebut) (Just dateFin) Nothing (Just "INFORMATIVE") now (length rows)
      insertGazConsos conn ingId rows
      return $ Right (length rows)

  -- Injections publiées
  rInj <- consulterInjectionsPubliees session (pceText pce) (ByDateRange dateDebut dateFin) >>= \case
    Left  err  -> return $ Left (adictErrorToText err)
    Right injs -> do
      let rows = mapMaybe (toGazInjection TDPubliee) injs
      ingId <- logGazIngestion conn "donnees_injections_publiees"
                 (Just dateDebut) (Just dateFin) Nothing (Just "PUBLIEE") now (length rows)
      insertGazInjections conn ingId rows
      return $ Right (length rows)

  -- Informations contractuelles
  rCont <- consulterDonneesContractuelles session (pceText pce) [] >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right retour -> do
      let info = toGazInfosContractuelles retour
      ingId <- logGazIngestion conn "donnees_contractuelles"
                 Nothing Nothing Nothing Nothing now 1
      insertGazInfosContractuelles conn ingId now info
      return $ Right True

  -- Informations techniques
  rTech <- consulterDonneesTechniques session (pceText pce) >>= \case
    Left  err    -> return $ Left (adictErrorToText err)
    Right retour -> do
      let info = toGazInfosTechniques retour
      ingId <- logGazIngestion conn "donnees_techniques"
                 Nothing Nothing Nothing Nothing now 1
      insertGazInfosTechniques conn ingId now info
      return $ Right True

  return $ AdictIngestReport pce rPub rInfo rInj rCont rTech
