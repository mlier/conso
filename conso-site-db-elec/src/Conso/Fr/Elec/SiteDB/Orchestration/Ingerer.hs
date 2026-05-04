{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Ingerer
  ( IngererElecParams(..)
  , IngererElecMode(..)
  , IngererElecReport(..)
  , PrmIngestionReport(..)
  , BackfillDemande(..)
  , ingererElec
  ) where

import           Control.Exception             (catch, SomeException, displayException)
import           Data.Char                     (digitToInt)
import           Data.List                     (nub, sortBy)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (mapMaybe)
import           Data.Ord                      (comparing)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
  ( Day, getCurrentTime, utctDay, addGregorianYearsRollOver )
import qualified Data.ByteString               as BS

import           Database.SQLite.Simple        (Connection)

import           Conso.Fr.Elec.Sge.Rfiles.LoadRFiles
  ( RFilesConfig(..), PostDownload(..), DayLimit(..), getConfig, loadRFiles )
import           Conso.Fr.Elec.Sge.Rfiles.DecryptRFiles
  ( DecryptConfig(..), decryptDir )

import           Conso.Fr.SiteDB.Types         (Prm(..), SiteId, SiteRef(..))
import           Conso.Fr.SiteDB.Registry.Operations (listSites)

import           Conso.Fr.Elec.SiteDB.Types.Common     (PrmId(..))
import           Conso.Fr.Elec.SiteDB.Storage.Connection (openSiteDbElec)
import           Conso.Fr.Elec.SiteDB.Storage.Query
  ( derniereHorodateCourbe, derniereDateEnergie, derniereDatePmax )
import           Conso.Fr.Elec.SiteDB.Storage.Gaps
  ( detectEnergyGaps, detectPmaxGaps )
import           Conso.Fr.Elec.SiteDB.Ingestion.FromRfiles
  ( ingestDirectory, IngestDirResult(..) )
import           Conso.Fr.Elec.SiteDB.Ingestion.Batch  (IngestResult(..))
import           Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillDemande(..), envoyerBackfill )


-- ---------------------------------------------------------------------------
-- Fenêtres temporelles M023

lookbackEnergiePmax :: Integer
lookbackEnergiePmax = 3

lookbackCourbes :: Integer
lookbackCourbes = 2


-- ---------------------------------------------------------------------------
-- Types

data IngererElecMode
  = ModeNormal    -- ^ Télécharger SFTP + déchiffrer + ingérer
  | ModeBackfill  -- ^ Détecter trous + envoyer demandes M023
  deriving (Show, Eq)

data IngererElecParams = IngererElecParams
  { iepPrmFilter    :: Maybe [Prm]
  , iepDayLimit     :: DayLimit
  , iepPostDownload :: PostDownload
  , iepMode         :: IngererElecMode
  }

data PrmIngestionReport = PrmIngestionReport
  { prirPrm             :: Prm
  , prirFichiersOk      :: Int
  , prirFichiersSkip    :: Int
  , prirErreurs         :: [(Text, Text)]
  , prirDerniereCourbe  :: Maybe Text
  , prirDerniereEnergie :: Maybe Text
  , prirDernierePmax    :: Maybe Text
  , prirTrousEnergie    :: [Day]
  , prirTrousPmax       :: [Day]
  } deriving (Show)

data IngererElecReport = IngererElecReport
  { ierFichiersTotal   :: Int
  , ierFichiersIgnores :: Int
  , ierFichiersErreur  :: Int
  , ierDetails         :: [PrmIngestionReport]
  , ierErrors          :: [(Prm, Text)]
  , ierBackfill        :: [BackfillDemande]
  , ierErreursParser   :: [(Text, Text)]  -- ^ (fichier, message) — erreurs de parsing JSON
  } deriving (Show)


-- ---------------------------------------------------------------------------
-- Implémentation principale

ingererElec
  :: Connection
  -> FilePath
  -> FilePath
  -> IngererElecParams
  -> IO IngererElecReport
ingererElec regConn configDir siteDbDir params = do
  today <- utctDay <$> getCurrentTime
  let start3Ans = addGregorianYearsRollOver (negate lookbackEnergiePmax) today
      start2Ans = addGregorianYearsRollOver (negate lookbackCourbes)     today

  sites <- listSites regConn
  let prmsSites = mapMaybe (\sr -> fmap (\p -> (p, srSiteId sr)) (srPrm sr)) sites
      prmsSites' = case iepPrmFilter params of
        Nothing   -> prmsSites
        Just filt -> filter (\(p, _) -> p `elem` filt) prmsSites

  case iepMode params of
    ModeNormal   -> runNormal configDir siteDbDir params prmsSites' start3Ans today
    ModeBackfill -> runBackfill siteDbDir prmsSites' start3Ans start2Ans today


-- ---------------------------------------------------------------------------
-- Mode normal : SFTP → déchiffrer → ingérer

runNormal
  :: FilePath -> FilePath -> IngererElecParams
  -> [(Prm, SiteId)] -> Day -> Day
  -> IO IngererElecReport
runNormal configDir siteDbDir params prmsSites start3Ans today = do
  cfg <- getConfig
  _   <- loadRFiles cfg (iepPostDownload params) (iepDayLimit params)
  decryptDir (decryptConfigFromRFiles cfg) (localDir cfg)
  dirResults <- ingestDirectory configDir siteDbDir (localDir cfg)

  let total        = length dirResults
      ignores      = length [ () | FileSkip _ _ <- dirResults ]
      errors       = length [ () | FileErr  _ _ <- dirResults ]
      byPrmRaw     = groupByPrm dirResults
      parseErrors  = Map.findWithDefault [] "?" byPrmRaw
      byPrm        = Map.delete "?" byPrmRaw

  details <- mapM (buildReport siteDbDir byPrm start3Ans today) prmsSites
  let (errPrms, okDetails) = partitionReports details

  return $ IngererElecReport
    { ierFichiersTotal   = total
    , ierFichiersIgnores = ignores
    , ierFichiersErreur  = errors
    , ierDetails         = sortBy (comparing ((\(Prm t) -> t) . prirPrm)) okDetails
    , ierErrors          = errPrms
    , ierBackfill        = []
    , ierErreursParser   = parseErrors
    }


-- ---------------------------------------------------------------------------
-- Mode backfill : détecter trous → envoyer M023

runBackfill
  :: FilePath -> [(Prm, SiteId)] -> Day -> Day -> Day
  -> IO IngererElecReport
runBackfill siteDbDir prmsSites start3Ans start2Ans today = do
  demandes <- concat <$> mapM (backfillPrm siteDbDir start3Ans start2Ans today) prmsSites
  return $ IngererElecReport
    { ierFichiersTotal   = 0
    , ierFichiersIgnores = 0
    , ierFichiersErreur  = 0
    , ierDetails         = []
    , ierErrors          = []
    , ierBackfill        = demandes
    , ierErreursParser   = []
    }

backfillPrm
  :: FilePath -> Day -> Day -> Day -> (Prm, SiteId)
  -> IO [BackfillDemande]
backfillPrm siteDbDir start3Ans start2Ans today (prm, siteId) =
  catch (backfillPrmUnsafe siteDbDir start3Ans start2Ans today prm siteId)
        (\e -> return
          [ BackfillDemande prm "R65/R66" (T.pack (show start3Ans)) (T.pack (show today))
              (Left (T.pack (displayException (e :: SomeException)))) ])

backfillPrmUnsafe
  :: FilePath -> Day -> Day -> Day -> Prm -> SiteId
  -> IO [BackfillDemande]
backfillPrmUnsafe siteDbDir start3Ans _start2Ans today prm siteId = do
  conn      <- openSiteDbElec siteDbDir siteId
  trousE    <- detectEnergyGaps conn "CONS" start3Ans today
  trousP    <- detectPmaxGaps   conn "CONS" start3Ans today
  let periodesEP = groupDays (nub (trousE ++ trousP))
  demandesEP <- mapM (envoyerBackfill prm "R65/R66" "ENERGIE") periodesEP
  return demandesEP


-- ---------------------------------------------------------------------------
-- Helpers

buildReport
  :: FilePath
  -> Map Text [(Text, Text)]
  -> Day -> Day
  -> (Prm, SiteId)
  -> IO (Either (Prm, Text) PrmIngestionReport)
buildReport siteDbDir byPrm start3Ans today (prm@(Prm prmText), siteId) =
  catch (Right <$> buildReportUnsafe siteDbDir byPrm start3Ans today prm prmText siteId)
        (\e -> return $ Left (prm, T.pack (displayException (e :: SomeException))))

buildReportUnsafe
  :: FilePath -> Map Text [(Text, Text)] -> Day -> Day -> Prm -> Text -> SiteId
  -> IO PrmIngestionReport
buildReportUnsafe siteDbDir byPrm start3Ans today prm prmText siteId = do
  conn <- openSiteDbElec siteDbDir siteId
  let entries     = Map.findWithDefault [] prmText byPrm
      fichiersOk  = length [ () | (_, e) <- entries, e == "ok"   ]
      fichiersSkp = length [ () | (_, e) <- entries, e == "skip" ]
      fichiersErr = [ (f, e) | (f, e) <- entries, e /= "ok", e /= "skip" ]

  dCourbe  <- derniereHorodateCourbe conn
  dEnergie <- derniereDateEnergie conn
  dPmax    <- derniereDatePmax conn

  trousE <- detectEnergyGaps conn "CONS" start3Ans today
  trousP <- detectPmaxGaps   conn "CONS" start3Ans today

  return $ PrmIngestionReport
    { prirPrm             = prm
    , prirFichiersOk      = fichiersOk
    , prirFichiersSkip    = fichiersSkp
    , prirErreurs         = fichiersErr
    , prirDerniereCourbe  = dCourbe
    , prirDerniereEnergie = dEnergie
    , prirDernierePmax    = dPmax
    , prirTrousEnergie    = trousE
    , prirTrousPmax       = trousP
    }

groupByPrm :: [IngestDirResult] -> Map Text [(Text, Text)]
groupByPrm = foldr step Map.empty
  where
    step (FileOk path results) m = foldr (addResult (T.pack path)) m results
    step (FileSkip _ _) m = m
    step (FileErr path err) m =
      Map.insertWith (<>) "?" [(T.pack path, err)] m

    addResult path (IngestOk   (PrmId pid) _) m =
      Map.insertWith (<>) pid [(path, "ok")]   m
    addResult path (IngestSkip (PrmId pid) _) m =
      Map.insertWith (<>) pid [(path, "skip")] m
    addResult path (IngestErr  (PrmId pid) e) m =
      Map.insertWith (<>) pid [(path, e)]      m

partitionReports
  :: [Either (Prm, Text) PrmIngestionReport]
  -> ([(Prm, Text)], [PrmIngestionReport])
partitionReports = foldr step ([], [])
  where
    step (Left  e) (es, ds) = (e:es, ds)
    step (Right d) (es, ds) = (es, d:ds)

-- | Regroupe des jours isolés en périodes contiguës (un seul gap = 1 période).
groupDays :: [Day] -> [(Day, Day)]
groupDays [] = []
groupDays (d:ds) = go d d ds
  where
    go gStart gEnd [] = [(gStart, gEnd)]
    go gStart gEnd (x:xs)
      | toEnum (fromEnum x) == succ gEnd = go gStart x xs
      | otherwise = (gStart, gEnd) : go x x xs

decryptConfigFromRFiles :: RFilesConfig -> DecryptConfig
decryptConfigFromRFiles cfg = DecryptConfig
  { dc128Key     = hexToBytes <$> zipAes128Key cfg
  , dc128IV      = hexToBytes <$> zipAes128IV  cfg
  , dc256Key     = hexToBytes <$> zipAes256Key cfg
  , dcSwitchDate = zipAesSwitchDate cfg
  }

hexToBytes :: String -> BS.ByteString
hexToBytes []         = BS.empty
hexToBytes [_]        = BS.empty
hexToBytes (a:b:rest) =
  BS.cons (fromIntegral (digitToInt a * 16 + digitToInt b)) (hexToBytes rest)
