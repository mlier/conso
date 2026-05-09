{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Conso.Fr.Elec.SiteDB.Orchestration.Ingerer
  ( IngererElecParams(..)
  , IngererElecReport(..)
  , PrmIngestionReport(..)
  , PrmInfoC68(..)
  , ingererElec
  ) where

import           Control.Exception             (catch, SomeException, displayException)
import           Data.Char                     (digitToInt)
import           Data.Either                   (partitionEithers)
import           Data.List                     (nub, sortBy)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (mapMaybe, catMaybes)
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
  , derniereDateEnergie, derniereDatePmax
  , queryLatestPrmInfo, PrmInfoRow(..) )
import           Conso.Fr.Elec.SiteDB.Storage.Gaps
  ( detectEnergyGaps, detectPmaxGaps, detectCurveDayGaps )
import           Conso.Fr.Elec.SiteDB.Ingestion.FromRfiles
  ( ingestDirectory, IngestDirResult(..) )
import           Conso.Fr.Elec.SiteDB.Ingestion.Batch  (IngestResult(..))
import           Conso.Fr.Elec.SiteDB.Orchestration.Backfill
  ( BackfillBesoin(..), BackfillBatch(..)
  , grouperBesoins, filtrerDejaDemandes
  , itcDejaDemandeeAujourdhui
  , envoyerBatchMfi, envoyerBatchItc
  , chunksOf )
import           Conso.Fr.Elec.SiteDB.Orchestration.CompteRendu
  ( CrResult(..), processCrDirectory )


-- ---------------------------------------------------------------------------
-- Fenêtres temporelles M023

lookbackEnergiePmax :: Integer
lookbackEnergiePmax = 3

lookbackCourbes :: Integer
lookbackCourbes = 2

-- Limites officielles Enedis par flux (PRMs par requête)
limiteMfi :: Text -> Int
limiteMfi "COURBES" = 1500
limiteMfi "INDEX"   = 1500
limiteMfi _         = 10000


-- ---------------------------------------------------------------------------
-- Types

data IngererElecParams = IngererElecParams
  { iepPrmFilter    :: Maybe [Prm]
  , iepDayLimit     :: DayLimit
  , iepPostDownload :: PostDownload
  }

data PrmInfoC68 = PrmInfoC68
  { picSegment            :: Maybe Text
  , picEtatContractuel    :: Maybe Text
  , picFormuleTarifaire   :: Maybe Text
  , picPuissanceSouscrite :: Maybe Text
  , picAdresse            :: Maybe Text
  , picMatriculeCompteur  :: Maybe Text
  , picLinky              :: Maybe Text
  , picTitulaireNom       :: Maybe Text
  } deriving (Show)

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
  , prirInfoC68         :: Maybe PrmInfoC68
  } deriving (Show)

data IngererElecReport = IngererElecReport
  { ierFichiersTotal   :: Int
  , ierFichiersIgnores :: Int
  , ierFichiersErreur  :: Int
  , ierDetails         :: [PrmIngestionReport]
  , ierErrors          :: [(Prm, Text)]
  , ierBackfill        :: [BackfillBatch]
  , ierCR              :: [CrResult]
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
  let avantHier = addDays (-2) today
      start3Ans = addGregorianYearsRollOver (negate lookbackEnergiePmax) avantHier
      start2Ans = addGregorianYearsRollOver (negate lookbackCourbes)     avantHier

  sites <- listSites regConn
  let prmsSites = mapMaybe (\sr -> fmap (, srSiteId sr) (srPrm sr)) sites
      prmsSites' = case iepPrmFilter params of
        Nothing   -> prmsSites
        Just filt -> filter (\(p, _) -> p `elem` filt) prmsSites

  cfg <- getConfig
  _   <- loadRFiles cfg (iepPostDownload params) (iepDayLimit params)
  decryptDir (decryptConfigFromRFiles cfg) (localDir cfg)
  dirResults <- ingestDirectory configDir siteDbDir (localDir cfg)

  let total       = length dirResults
      ignores     = length [ () | FileSkip _ _ <- dirResults ]
      errors      = length [ () | FileErr  _ _ <- dirResults ]
      byPrmRaw    = groupByPrm dirResults
      parseErrors = Map.findWithDefault [] "?" byPrmRaw
      byPrm       = Map.delete "?" byPrmRaw

  let rfilesDir = localDir cfg
  -- Phase 1 : collecter rapports + besoins pour chaque PRM
  results <- mapM (buildReport rfilesDir siteDbDir byPrm start3Ans start2Ans avantHier) prmsSites'
  let (errPrms, okTriples)           = partitionEithers results
      (okReports, allBesoins, allCRs) = unzip3 okTriples

  -- Phase 2-4 : grouper, dédupliquer, envoyer
  let besoinsTous = concat allBesoins
  batches <- envoyerDemandes siteDbDir prmsSites' besoinsTous avantHier

  return $ IngererElecReport
    { ierFichiersTotal   = total
    , ierFichiersIgnores = ignores
    , ierFichiersErreur  = errors
    , ierDetails         = sortBy (comparing ((\(Prm t) -> t) . prirPrm)) okReports
    , ierErrors          = errPrms
    , ierBackfill        = batches
    , ierCR              = concat allCRs
    , ierErreursParser   = parseErrors
    }


-- ---------------------------------------------------------------------------
-- Phase 1 : rapport par PRM (sans envoi de requêtes)

buildReport
  :: FilePath
  -> FilePath
  -> Map Text [(Text, Text)]
  -> Day -> Day -> Day
  -> (Prm, SiteId)
  -> IO (Either (Prm, Text) (PrmIngestionReport, [BackfillBesoin], [CrResult]))
buildReport rfilesDir siteDbDir byPrm start3Ans start2Ans endDate (prm, siteId) =
  catch (Right <$> buildReportUnsafe rfilesDir siteDbDir byPrm start3Ans start2Ans endDate prm siteId)
        (\e -> return $ Left (prm, T.pack (displayException (e :: SomeException))))

buildReportUnsafe
  :: FilePath -> FilePath -> Map Text [(Text, Text)] -> Day -> Day -> Day -> Prm -> SiteId
  -> IO (PrmIngestionReport, [BackfillBesoin], [CrResult])
buildReportUnsafe rfilesDir siteDbDir byPrm start3Ans start2Ans endDate prm@(Prm prmText) siteId = do
  conn <- openSiteDbElec siteDbDir siteId
  let entries     = Map.findWithDefault [] prmText byPrm
      fichiersOk  = length [ () | (_, e) <- entries, e == "ok"   ]
      fichiersSkp = length [ () | (_, e) <- entries, e == "skip" ]
      fichiersErr = [ (f, e) | (f, e) <- entries, e /= "ok", e /= "skip" ]

  dCourbe  <- derniereHorodateCourbe conn
  dEnergie <- derniereDateEnergie conn
  dPmax    <- derniereDatePmax conn
  dIndex   <- derniereHorodateIndex conn

  trousE <- detectEnergyGaps   conn "CONS" start3Ans endDate
  trousP <- detectPmaxGaps     conn "CONS" start3Ans endDate
  trousC <- detectCurveDayGaps conn "CONS" start2Ans endDate

  mInfoRow <- queryLatestPrmInfo conn
  let infoC68 = fmap rowToC68Summary mInfoRow

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
        , prirInfoC68         = infoC68
        }

  let besoinsE = [BackfillBesoin prm "ENERGIE" "R65" d f | (d, f) <- groupDays trousE]
      besoinsP = [BackfillBesoin prm "PMAX"    "R66" d f | (d, f) <- groupDays trousP]
      besoinsC = [BackfillBesoin prm "COURBES" "R63" d f | (d, f) <- groupDays (nub trousC)]
      besoinsI = besoinsIndex prm start3Ans endDate dIndex

  crResults <- processCrDirectory rfilesDir conn

  return (report, besoinsE ++ besoinsP ++ besoinsC ++ besoinsI, crResults)

besoinsIndex :: Prm -> Day -> Day -> Maybe Text -> [BackfillBesoin]
besoinsIndex prm start3Ans endDate mLastDate =
  case mLastDate of
    Nothing -> [BackfillBesoin prm "INDEX" "R64" start3Ans endDate]
    Just t  -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" (take 10 (T.unpack t)) of
      Nothing -> [BackfillBesoin prm "INDEX" "R64" start3Ans endDate]
      Just d  ->
        let debut = addDays 1 d
        in [BackfillBesoin prm "INDEX" "R64" debut endDate | debut <= endDate]


-- ---------------------------------------------------------------------------
-- Phases 2-4 : grouper, dédupliquer, envoyer

envoyerDemandes
  :: FilePath
  -> [(Prm, SiteId)]
  -> [BackfillBesoin]
  -> Day
  -> IO [BackfillBatch]
envoyerDemandes siteDbDir prmsSites besoins _endDate = do
  let grouped = grouperBesoins besoins

  -- MFI : un batch par (typeCode, debut, fin), découpé selon les limites
  mfiBatches <- concat <$> mapM (envoyerGroupe siteDbDir prmsSites) (Map.toList grouped)

  -- ITC C68 : une fois par jour, tous les PRMs
  itcBatches <- case prmsSites of
    [] -> return []
    ((_, firstSiteId) : _) -> do
      conn <- openSiteDbElec siteDbDir firstSiteId
      dejaDemandee <- itcDejaDemandeeAujourdhui conn
      if dejaDemandee
        then return []
        else do
          let tousLesPrms = map fst prmsSites
              chunks = chunksOf 10000 tousLesPrms
          catMaybes <$> mapM (envoyerBatchItc conn) chunks

  return (mfiBatches ++ itcBatches)

envoyerGroupe
  :: FilePath
  -> [(Prm, SiteId)]
  -> ((Text, Day, Day), (Text, [Prm]))
  -> IO [BackfillBatch]
envoyerGroupe siteDbDir prmsSites ((typeCode, debut, fin), (flux, prms)) = do
  case lookupSiteId (head prms) prmsSites of
    Nothing     -> return []
    Just siteId -> do
      conn <- openSiteDbElec siteDbDir siteId
      filtres <- filtrerDejaDemandes conn typeCode debut fin prms
      let lim    = limiteMfi typeCode
          chunks = chunksOf lim filtres
      catMaybes <$> mapM (\chunk -> envoyerBatchMfi conn chunk flux typeCode (debut, fin)) chunks

lookupSiteId :: Prm -> [(Prm, SiteId)] -> Maybe SiteId
lookupSiteId prm = fmap snd . safeHead . filter ((== prm) . fst)
  where safeHead []    = Nothing
        safeHead (x:_) = Just x


-- ---------------------------------------------------------------------------
-- Helpers

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

rowToC68Summary :: PrmInfoRow -> PrmInfoC68
rowToC68Summary r = PrmInfoC68
  { picSegment            = piSegment r
  , picEtatContractuel    = piEtatContractuel r
  , picFormuleTarifaire   = piFormuleTarifaireCode r
  , picPuissanceSouscrite = piPuissanceSouscrite r
  , picAdresse            = buildAdresse r
  , picMatriculeCompteur  = piMatriculeCompteur r
  , picLinky              = piDatePremierePoseLinky r
  , picTitulaireNom       = buildTitulaireNom r
  }
  where
    buildAdresse row = case (piAdresseNumeroNomVoie row, piAdresseCodePostal row, piAdresseCommune row) of
      (Just rue, Just cp, Just commune) -> Just (rue <> " " <> cp <> " " <> commune)
      _                                 -> Nothing
    buildTitulaireNom row = case (piTitulaireNom row, piTitulairePrenom row) of
      (Just nom, Just prenom) -> Just (prenom <> " " <> nom)
      (Just nom, Nothing)     -> Just nom
      _                       -> piTitulaireDenominationSociale row
