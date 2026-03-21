{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Ingestion.Batch
Description : Ingestion transactionnelle de fichiers JSON SGE par lot

Fournit 'ingestFile' pour ingérer un fichier unique et 'ingestBatch' pour
un lot de fichiers. Chaque PRM est traité dans une transaction distincte :
log d'ingestion + insertions métier sont atomiques.

Un 'IngestResult' est produit par PRM présent dans le flux. En cas d'erreur
(parsing ou base), la transaction est annulée et un 'IngestErr' est retourné
sans interrompre les autres PRM.
-}
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

-- | Résultat de l'ingestion pour un PRM.
data IngestResult
  = IngestOk  PrmId Text -- ^ Succès : PRM ingéré + code flux (ex. @\"R63\"@)
  | IngestErr PrmId Text -- ^ Échec  : PRM + message d'erreur
  deriving (Show)

-- | Ingère un fichier JSON SGE dans les bases SQLite des PRM qu'il contient.
--
-- Le 'CodeFlux' est fourni par l'appelant (déterminé par le contexte de récupération).
-- La log d'ingestion et toutes les insertions métier se font dans une transaction
-- unique par PRM. Retourne un 'IngestResult' par PRM présent dans le flux.
ingestFile
  :: FilePath    -- ^ Répertoire racine des bases SQLite (sharding 3×3)
  -> CodeFlux    -- ^ Type de flux à ingérer
  -> Maybe Text  -- ^ Nom du fichier source (pour traçabilité dans @ingestion_log@)
  -> ByteString  -- ^ Contenu JSON du fichier
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

-- | Ingère un lot de fichiers JSON de façon séquentielle.
-- Equivalent à @concat \<$\> mapM (uncurry3 ingestFile) files@.
-- Les erreurs sur un fichier n'interrompent pas les suivants.
ingestBatch
  :: FilePath                           -- ^ Répertoire racine des bases
  -> [(CodeFlux, Maybe Text, ByteString)] -- ^ Liste de (codeFlux, nom fichier, contenu)
  -> IO [IngestResult]
ingestBatch baseDir files = do
  results <- mapM (\(cf, src, bs) -> ingestFile baseDir cf src bs) files
  return (concat results)

modePublicationToText :: ModePublication -> Text
modePublicationToText MP_Ponctuel    = "P"
modePublicationToText MP_Quotidien   = "Q"
modePublicationToText MP_Hebdomadaire = "H"
modePublicationToText MP_Mensuel     = "M"
