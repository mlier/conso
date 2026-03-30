{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-|
Module      : Conso.Fr.Site.SiteDB.Gaz.Ingestion.FromApi
Description : Ingestion des données gaz depuis l'API GRDF ADICT

Orchestre l'appel aux endpoints ADICT, l'ouverture de la base site et
l'insertion des données gaz dans les tables @gaz_*@.

Les fonctions d'appel API sont injectées par l'appelant via 'AdictFetchCallbacks',
évitant toute dépendance circulaire entre @conso-site-db@ et @conso-gaz-adict@.

Les callbacks reçoivent des types déjà convertis ('GazConso', 'GazInjection', …)
— la conversion @ConsoRestit → GazConso@ est faite côté @conso-gaz-adict@.

Usage typique depuis @conso-gaz-adict@ :

@
let callbacks = AdictFetchCallbacks
      { fetchConsosPubliees = \\pce d1 d2 ->
          bimap show (map toGazConso TDPubliee) \<$\>
            consulterConsosPubliees session (pceTxt pce) (ByDateRange d1 d2)
      , ...
      }
report <- ingestFromAdict callbacks "~\/.conso" "~\/.conso\/sites"
            (Pce "12345678901234") "2024-01-01" "2024-12-31"
@
-}
module Conso.Fr.Site.SiteDB.Gaz.Ingestion.FromApi
  ( AdictFetchCallbacks(..)
  , AdictIngestReport(..)
  , ingestFromAdict
  ) where

import           Data.Text              (Text)
import           Data.Time              (getCurrentTime)

import           Conso.Fr.Site.Types                               (Pce(..))
import           Conso.Fr.Site.Registry                            (openRegistry, lookupOrCreateByPce)
import           Conso.Fr.Site.SiteDB.Elec.Storage.Connection      (openSiteDb)
import           Conso.Fr.Site.SiteDB.Gaz.Types
import           Conso.Fr.Site.SiteDB.Gaz.Storage.Insert

-- | Callbacks d'appel API ADICT, fournis par @conso-gaz-adict@.
-- Chaque fonction retourne soit une erreur (@Left Text@) soit les données converties.
data AdictFetchCallbacks = AdictFetchCallbacks
  { -- | Consommations publiées — déjà converties en 'GazConso' avec TypeDonnee=TDPubliee
    fetchConsosPubliees :: Pce -> Text -> Text -> IO (Either Text [GazConso])
    -- | Consommations informatives — déjà converties avec TypeDonnee=TDInformative
  , fetchConsosInfos    :: Pce -> Text -> Text -> IO (Either Text [GazConso])
    -- | Injections publiées
  , fetchInjections     :: Pce -> Text -> Text -> IO (Either Text [GazInjection])
    -- | Informations contractuelles (un seul objet par PCE)
  , fetchInfosContractuelles :: Pce -> IO (Either Text (Maybe GazInfosContractuelles))
    -- | Informations techniques (un seul objet par PCE)
  , fetchInfosTechniques     :: Pce -> IO (Either Text (Maybe GazInfosTechniques))
  }

-- | Rapport d'une opération d'ingestion ADICT.
data AdictIngestReport = AdictIngestReport
  { airPce              :: Pce   -- ^ PCE traité
  , airConsosPubliees   :: Either Text Int  -- ^ nb consommations publiées insérées ou erreur
  , airConsosInfos      :: Either Text Int  -- ^ nb consommations informatives insérées ou erreur
  , airInjections       :: Either Text Int  -- ^ nb injections insérées ou erreur
  , airInfosContract    :: Either Text Bool -- ^ infos contractuelles insérées ou erreur
  , airInfosTech        :: Either Text Bool -- ^ infos techniques insérées ou erreur
  } deriving (Show)

-- | Ingère toutes les données ADICT pour un PCE sur une période.
-- Ouvre/crée le site via le registre, puis insère chaque type de donnée.
ingestFromAdict
  :: AdictFetchCallbacks
  -> FilePath -- ^ Répertoire de configuration (contient @registry.db@)
  -> FilePath -- ^ Répertoire des bases SQLite site
  -> Pce      -- ^ PCE à ingérer
  -> Text     -- ^ Date de début (YYYY-MM-DD)
  -> Text     -- ^ Date de fin (YYYY-MM-DD)
  -> IO AdictIngestReport
ingestFromAdict callbacks configDir siteDbDir pce dateDebut dateFin = do
  reg    <- openRegistry configDir
  siteId <- lookupOrCreateByPce reg pce
  conn   <- openSiteDb siteDbDir siteId
  now    <- getCurrentTime

  -- Consommations publiées
  rPub  <- fetchConsosPubliees callbacks pce dateDebut dateFin >>= \case
    Left  err   -> return $ Left err
    Right consos -> do
      ingId <- logGazIngestion conn "donnees_consos_publiees"
                 (Just dateDebut) (Just dateFin) Nothing (Just "PUBLIEE") now (length consos)
      insertGazConsos conn ingId consos
      return $ Right (length consos)

  -- Consommations informatives
  rInfo <- fetchConsosInfos callbacks pce dateDebut dateFin >>= \case
    Left  err   -> return $ Left err
    Right consos -> do
      ingId <- logGazIngestion conn "donnees_consos_informatives"
                 (Just dateDebut) (Just dateFin) Nothing (Just "INFORMATIVE") now (length consos)
      insertGazConsos conn ingId consos
      return $ Right (length consos)

  -- Injections publiées
  rInj  <- fetchInjections callbacks pce dateDebut dateFin >>= \case
    Left  err  -> return $ Left err
    Right injs -> do
      ingId <- logGazIngestion conn "donnees_injections_publiees"
                 (Just dateDebut) (Just dateFin) Nothing (Just "PUBLIEE") now (length injs)
      insertGazInjections conn ingId injs
      return $ Right (length injs)

  -- Informations contractuelles
  rCont <- fetchInfosContractuelles callbacks pce >>= \case
    Left err          -> return $ Left err
    Right Nothing     -> return $ Right False
    Right (Just info) -> do
      ingId <- logGazIngestion conn "donnees_contractuelles"
                 Nothing Nothing Nothing Nothing now 1
      insertGazInfosContractuelles conn ingId now info
      return $ Right True

  -- Informations techniques
  rTech <- fetchInfosTechniques callbacks pce >>= \case
    Left err          -> return $ Left err
    Right Nothing     -> return $ Right False
    Right (Just info) -> do
      ingId <- logGazIngestion conn "donnees_techniques"
                 Nothing Nothing Nothing Nothing now 1
      insertGazInfosTechniques conn ingId now info
      return $ Right True

  return $ AdictIngestReport pce rPub rInfo rInj rCont rTech
