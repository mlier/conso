{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Ingestion.Batch
  ( ingestFile
  , ingestBatch
  , IngestResult(..)
  ) where

import           Database.SQLite.Simple
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime, UTCTime)
import           Data.Time.Format       (formatTime, defaultTimeLocale)
import           Control.Exception      (try, SomeException)
import           Conso.Fr.Elec.SgeDB.Types.Common
import           Conso.Fr.Elec.SgeDB.Types.Header
import           Conso.Fr.Elec.SgeDB.Types.R63    (FluxR63(..), MesureR63(..))
import           Conso.Fr.Elec.SgeDB.Types.R64    (FluxR64(..), MesureR64(..))
import           Conso.Fr.Elec.SgeDB.Types.R65    (FluxR65(..), MesureR65(..))
import           Conso.Fr.Elec.SgeDB.Types.R66    (FluxR66(..), MesureR66(..))
import           Conso.Fr.Elec.SgeDB.Types.R67    (FluxR67(..), MesureR67(..))
import           Conso.Fr.Elec.SgeDB.Types.C68
import           Conso.Fr.Elec.SgeDB.Ingestion.Parser
import           Conso.Fr.Elec.SgeDB.Ingestion.Versioning ()
import           Conso.Fr.Elec.SgeDB.Storage.Connection   (openPrmDb)
import           Conso.Fr.Elec.SgeDB.Storage.Insert

data IngestResult
  = IngestOk  PrmId Text  -- PRM + code flux
  | IngestErr PrmId Text  -- PRM + message d'erreur
  deriving (Show)

-- | Ingère un fichier JSON dans la base du PRM.
-- Le CodeFlux est passé explicitement (déterminé par le contexte appelant).
-- Toute l'ingestion (log + insertions) se fait dans une transaction unique.
ingestFile
  :: FilePath    -- répertoire de bases SQLite
  -> CodeFlux
  -> Maybe Text  -- fichier source (pour log)
  -> ByteString  -- contenu JSON
  -> IO [IngestResult]
ingestFile baseDir cf mSrc bs = do
  now <- getCurrentTime
  case parseFluxRxx cf bs of
    Left  err  -> return [IngestErr (PrmId "?") err]
    Right flux -> ingestFlux baseDir cf mSrc now flux

ingestFlux :: FilePath -> CodeFlux -> Maybe Text -> UTCTime -> FluxRxx -> IO [IngestResult]
ingestFlux baseDir cf mSrc now (FluxCourbeCharge f) =
  mapM (ingestMesure baseDir cf mSrc now f) (r63Mesures f)
  where
    ingestMesure bd _cf src t flux m = do
      let prm = mr63IdPrm m
          hdr = r63Header flux
      doInsert bd prm $ \conn -> do
        let p = mr63Periode m
        ingId <- logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr)
          (hIdPublication hdr)
          Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p))
          src
        insertCurvePoints conn ingId m
ingestFlux baseDir cf mSrc now (FluxIndex f) =
  mapM ingestM (r64Mesures f)
  where
    hdr = r64Header f
    ingestM m = do
      let prm = mr64IdPrm m
      doInsert baseDir prm $ \conn -> do
        let p = mr64Periode m
        ingId <- logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) (hIdPublication hdr) Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        insertIndexValues conn ingId m
ingestFlux baseDir cf mSrc now (FluxEnergie f) =
  mapM ingestM (r65Mesures f)
  where
    hdr = r65Header f
    ingestM m = do
      let prm = mr65IdPrm m
      doInsert baseDir prm $ \conn -> do
        let p = mr65Periode m
        ingId <- logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) Nothing Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        insertDailyEnergy conn ingId m
ingestFlux baseDir cf mSrc now (FluxPmax f) =
  mapM ingestM (r66Mesures f)
  where
    hdr = r66Header f
    ingestM m = do
      let prm = mr66IdPrm m
      doInsert baseDir prm $ \conn -> do
        let p = mr66Periode m
        ingId <- logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) (hIdPublication hdr) Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        insertDailyPmax conn ingId m
ingestFlux baseDir cf mSrc now (FluxFacturant f) =
  mapM ingestM (r67Mesures f)
  where
    hdr = r67Header f
    ingestM m = do
      let prm = mr67IdPrm m
      doInsert baseDir prm $ \conn -> do
        let p = mr67Periode m
        ingId <- logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) Nothing Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        insertBillingMeasures conn ingId m
ingestFlux baseDir cf mSrc now (FluxITC items) =
  mapM ingestM items
  where
    ingestM itc = do
      let prm = c68IdPrm itc
      doInsert baseDir prm $ \conn -> do
        ingId <- logIngestion conn cf "P" "C68" Nothing Nothing now
          Nothing Nothing mSrc
        insertPrmInfo conn ingId now itc

-- | Ouvre la connexion PRM, exécute l'action dans une transaction, ferme.
doInsert :: FilePath -> PrmId -> (Connection -> IO ()) -> IO IngestResult
doInsert baseDir prm action = do
  result <- try $ do
    conn <- openPrmDb baseDir prm
    withTransaction conn (action conn)
    close conn
  return $ case result of
    Left  ex -> IngestErr prm (T.pack (show (ex :: SomeException)))
    Right _  -> IngestOk  prm (unPrmId prm)

-- | Ingère un batch de fichiers (liste de (codeFlux, contenu))
ingestBatch
  :: FilePath
  -> [(CodeFlux, Maybe Text, ByteString)]
  -> IO [IngestResult]
ingestBatch baseDir files = do
  results <- mapM (\(cf, src, bs) -> ingestFile baseDir cf src bs) files
  return (concat results)

modePublicationToText :: ModePublication -> Text
modePublicationToText MP_Ponctuel    = "P"
modePublicationToText MP_Quotidien   = "Q"
modePublicationToText MP_Hebdomadaire = "H"
modePublicationToText MP_Mensuel     = "M"
