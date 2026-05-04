{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Ingerer
  ( IngererElecParams(..)
  , IngererElecReport(..)
  , PrmIngestionReport(..)
  , BackfillDemande(..)
  , ingererElec
  ) where

import           Control.Exception             (catch, SomeException, displayException)
import           Data.Char                     (digitToInt)
import           Data.Either                   (partitionEithers)
import           Data.List                     (nub, sortBy)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (mapMaybe, catMaybes, maybeToList)
import           Data.Ord                      (comparing)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time
  ( Day, getCurrentTime, utctDay, addGregorianYearsRollOver, addDays )
import           Data.Time.Format              (parseTimeM, defaultTimeLocale)
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
  ( derniereHorodateCourbe, derniereHorodateIndex
  , derniereDateEnergie, derniereDatePmax )
import           Conso.Fr.Elec.SiteDB.Storage.Gaps
  ( detectEnergyGaps, detectPmaxGaps, detectCurveDayGaps )
import           Conso.Fr.Elec.SiteDB.Ingestion.FromRfiles
  ( ingestDirectory, IngestDirResult(..) )
import           Conso.Fr.Elec.SiteDB.Ingestion.Batch  (IngestResult(..))
import           Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillDemande(..), envoyerSiNonRecent )


-- ---------------------------------------------------------------------------
-- Fenêtres temporelles M023

lookbackEnergiePmax :: Integer
lookbackEnergiePmax = 3

lookbackCourbes :: Integer
lookbackCourbes = 2


-- ---------------------------------------------------------------------------
-- Types

data IngererElecParams = IngererElecParams
  { iepPrmFilter    :: Maybe [Prm]
  , iepDayLimit     :: DayLimit
  , iepPostDownload :: PostDownload
  }

data PrmIngestionReport = PrmIngestionReport
  { prirPrm             :: Prm
  , prirFichiersOk      :: Int
  , prirFichiersSkip    :: Int
  , prirErreurs         :: [(Text, Text)]
  , prirDerniereCourbe  :: Maybe Text
  , prirDerniereEnergie :: Maybe Text
  , prirDernierePmax    :: Maybe Text
  , prirDerniereIndex   :: Maybe Text
  , prirTrousEnergie    :: [Day]
  , prirTrousPmax       :: [Day]
  , prirTrousCourbes    :: [Day]
  } deriving (Show)

data IngererElecReport = IngererElecReport
  { ierFichiersTotal   :: Int
  , ierFichiersIgnores :: Int
  , ierFichiersErreur  :: Int
  , ierDetails         :: [PrmIngestionReport]
  , ierErrors          :: [(Prm, Text)]
  , ierBackfill        :: [BackfillDemande]
  , ierErreursParser   :: [(Text, Text)]
  } deriving (Show)


-- ---------------------------------------------------------------------------
-- Pipeline principal

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

  results <- mapM (buildAndBackfill siteDbDir byPrm start3Ans start2Ans today) prmsSites'
  let (errPrms, okPairs)        = partitionEithers results
      (okReports, allDemandes)  = unzip okPairs

  return $ IngererElecReport
    { ierFichiersTotal   = total
    , ierFichiersIgnores = ignores
    , ierFichiersErreur  = errors
    , ierDetails         = sortBy (comparing ((\(Prm t) -> t) . prirPrm)) okReports
    , ierErrors          = errPrms
    , ierBackfill        = concat allDemandes
    , ierErreursParser   = parseErrors
    }


-- ---------------------------------------------------------------------------
-- Rapport + backfill par PRM (une seule connexion)

buildAndBackfill
  :: FilePath
  -> Map Text [(Text, Text)]
  -> Day -> Day -> Day
  -> (Prm, SiteId)
  -> IO (Either (Prm, Text) (PrmIngestionReport, [BackfillDemande]))
buildAndBackfill siteDbDir byPrm start3Ans start2Ans today (prm, siteId) =
  catch (Right <$> buildAndBackfillUnsafe siteDbDir byPrm start3Ans start2Ans today prm siteId)
        (\e -> return $ Left (prm, T.pack (displayException (e :: SomeException))))

buildAndBackfillUnsafe
  :: FilePath -> Map Text [(Text, Text)] -> Day -> Day -> Day -> Prm -> SiteId
  -> IO (PrmIngestionReport, [BackfillDemande])
buildAndBackfillUnsafe siteDbDir byPrm start3Ans start2Ans today prm@(Prm prmText) siteId = do
  conn <- openSiteDbElec siteDbDir siteId
  let entries     = Map.findWithDefault [] prmText byPrm
      fichiersOk  = length [ () | (_, e) <- entries, e == "ok"   ]
      fichiersSkp = length [ () | (_, e) <- entries, e == "skip" ]
      fichiersErr = [ (f, e) | (f, e) <- entries, e /= "ok", e /= "skip" ]

  dCourbe  <- derniereHorodateCourbe conn
  dEnergie <- derniereDateEnergie conn
  dPmax    <- derniereDatePmax conn
  dIndex   <- derniereHorodateIndex conn

  trousE <- detectEnergyGaps   conn "CONS" start3Ans today
  trousP <- detectPmaxGaps     conn "CONS" start3Ans today
  trousC <- detectCurveDayGaps conn "CONS" start2Ans today

  let report = PrmIngestionReport
        { prirPrm             = prm
        , prirFichiersOk      = fichiersOk
        , prirFichiersSkip    = fichiersSkp
        , prirErreurs         = fichiersErr
        , prirDerniereCourbe  = dCourbe
        , prirDerniereEnergie = dEnergie
        , prirDernierePmax    = dPmax
        , prirDerniereIndex   = dIndex
        , prirTrousEnergie    = trousE
        , prirTrousPmax       = trousP
        , prirTrousCourbes    = trousC
        }

  demandesEP <- mapMaybeM (envoyerSiNonRecent conn prm "R65/R66" "ENERGIE")
                  (groupDays (nub (trousE ++ trousP)))
  demandesC  <- mapMaybeM (envoyerSiNonRecent conn prm "R63" "COURBES")
                  (groupDays (nub trousC))
  demandesI  <- backfillIndexSiNecessaire conn prm start3Ans today dIndex

  return (report, demandesEP ++ demandesC ++ demandesI)


backfillIndexSiNecessaire
  :: Connection -> Prm -> Day -> Day -> Maybe Text
  -> IO [BackfillDemande]
backfillIndexSiNecessaire conn prm start3Ans today mLastDate = do
  let needsBackfill = case mLastDate of
        Nothing -> True
        Just t  -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
          Nothing -> True
          Just d  -> addDays 90 d < today
  if needsBackfill
    then maybeToList <$> envoyerSiNonRecent conn prm "R64" "INDEX" (start3Ans, today)
    else return []


-- ---------------------------------------------------------------------------
-- Helpers

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

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

-- | Regroupe des jours isolés en périodes contiguës.
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
