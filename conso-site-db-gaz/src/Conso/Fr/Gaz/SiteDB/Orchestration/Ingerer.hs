{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Gaz.SiteDB.Orchestration.Ingerer
  ( IngererGazParams(..)
  , IngererGazReport(..)
  , PceIngestionReport(..)
  , ingererGaz
  ) where

import           Control.Exception             (catch, SomeException, displayException)
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
  ( getCurrentTime, utctDay, addGregorianYearsRollOver
  , parseTimeM, formatTime, defaultTimeLocale )

import           Database.SQLite.Simple        (Connection)

import           Conso.Fr.Gaz.Adict.Adict      (AdictSession)
import           Conso.Fr.SiteDB.Types         (SiteId, Pce(..), SiteRef(..))
import           Conso.Fr.SiteDB.Registry.Operations (listSites)
import           Conso.Fr.Gaz.SiteDB.Storage.Connection (openSiteDbGaz)
import           Conso.Fr.Gaz.SiteDB.Storage.Query
import           Conso.Fr.Gaz.SiteDB.Ingestion.FromApi
import           Conso.Fr.Gaz.SiteDB.Types     (PeriodeGaz(..))


-- ---------------------------------------------------------------------------
-- Types

data IngererGazParams = IngererGazParams
  { igpPceFilter :: Maybe [Pce]  -- ^ Nothing = tous les PCEs du registre
  }

data PceIngestionReport = PceIngestionReport
  { pirPce            :: Pce
  , pirConsoPub       :: Either Text Int
  , pirConsoInfo      :: Either Text Int
  , pirInjections     :: Either Text Int
  , pirContractuelles :: Either Text ChangementInfosContract
  , pirTechniques     :: Either Text ChangementInfosTech
  , pirTrous          :: [(Text, Text)]
  } deriving (Show)

data IngererGazReport = IngererGazReport
  { igrTotal   :: Int
  , igrDetails :: [PceIngestionReport]
  , igrErrors  :: [(Pce, Text)]
  } deriving (Show)


-- ---------------------------------------------------------------------------
-- Implémentation

-- | Ingère les données GRDF ADICT pour tous les PCEs inscrits dans le registre.
-- Prend une connexion registre déjà ouverte pour éviter de la rouvrir par PCE.
ingererGaz
  :: Connection    -- ^ Connexion registre (déjà ouverte)
  -> AdictSession
  -> FilePath      -- ^ Répertoire des bases SQLite site ({uuid}.db)
  -> IngererGazParams
  -> IO IngererGazReport
ingererGaz regConn session siteDbDir params = do
  sites <- listSites regConn
  let pcesSites = mapMaybe pceAvecSite sites
      pcesSites' = case igpPceFilter params of
        Nothing   -> pcesSites
        Just filt -> filter (\(_, p) -> p `elem` filt) pcesSites
  resultats <- mapM (ingererUnPce session siteDbDir) pcesSites'
  let (erreurs, details) = partitionner resultats
  return $ IngererGazReport (length pcesSites') details erreurs
  where
    pceAvecSite sr = fmap (\p -> (srSiteId sr, p)) (srPce sr)

partitionner :: [Either (Pce, Text) PceIngestionReport]
             -> ([(Pce, Text)], [PceIngestionReport])
partitionner = foldr step ([], [])
  where
    step (Left  e) (es, ds) = (e:es, ds)
    step (Right d) (es, ds) = (es, d:ds)

ingererUnPce
  :: AdictSession -> FilePath -> (SiteId, Pce)
  -> IO (Either (Pce, Text) PceIngestionReport)
ingererUnPce session siteDbDir (siteId, pce) =
  catch (Right <$> ingererUnPceUnsafe session siteDbDir siteId pce)
        (\e -> return $ Left (pce, T.pack (displayException (e :: SomeException))))

ingererUnPceUnsafe
  :: AdictSession -> FilePath -> SiteId -> Pce -> IO PceIngestionReport
ingererUnPceUnsafe session siteDbDir siteId pce = do
  conn  <- openSiteDbGaz siteDbDir siteId
  today <- T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . utctDay <$> getCurrentTime

  debutPub  <- dateDebut conn "donnees_consos_publiees"   today 5
  debutInfo <- dateDebut conn "donnees_consos_informatives" today 3
  debutInj  <- dateDebut conn "donnees_injections_publiees" today 5

  rPub  <- ingererConsosPubliees      session conn pce debutPub  today
  rInfo <- ingererConsosInfos         session conn pce debutInfo today
  rInj  <- ingererInjections          session conn pce debutInj  today
  rCont <- ingererInfosContractuelles session conn pce
  rTech <- ingererInfosTechniques     session conn pce

  trous <- detectionTrous conn debutPub today PJournalier

  return $ PceIngestionReport pce rPub rInfo rInj rCont rTech trous

-- | Calcule la date de début pour un endpoint :
-- - Si une ingestion précédente existe, repart de sa date_fin
-- - Sinon, remonte de N années en arrière depuis aujourd'hui
dateDebut :: Connection -> Text -> Text -> Int -> IO Text
dateDebut conn endpoint today nAns = do
  mDerniere <- derniereIngestDate conn endpoint
  return $ case mDerniere of
    Just d  -> d
    Nothing -> soustraireAns today nAns

soustraireAns :: Text -> Int -> Text
soustraireAns todayStr n =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack todayStr) of
    Nothing  -> todayStr
    Just day -> T.pack $ formatTime defaultTimeLocale "%Y-%m-%d"
                  (addGregorianYearsRollOver (negate (fromIntegral n)) day)
