{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Ingestion.Batch
Description : Ingestion transactionnelle de fichiers JSON SGE par lot

Fournit 'ingestFile' pour ingérer un fichier unique et 'ingestBatch' pour
un lot de fichiers. Chaque PRM est traité dans une transaction distincte :
log d'ingestion + insertions métier sont atomiques.

Un 'IngestResult' est produit par PRM présent dans le flux. En cas d'erreur
(parsing ou base), la transaction est annulée et un 'IngestErr' est retourné
sans interrompre les autres PRM.

La fonction de connexion @openConn :: PrmId -> IO Connection@ est fournie
par l'appelant, typiquement via le registre central (PRM → UUID → chemin .db).
-}
module Conso.Fr.Elec.SiteDB.Ingestion.Batch
  ( ingestFile
  , ingestBatch
  , IngestResult(..)
  ) where

import           Database.SQLite.Simple
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (getCurrentTime, UTCTime)
import           Control.Exception      (try, SomeException)
import           Conso.Fr.Elec.SiteDB.Types.Common
import           Conso.Fr.Elec.SiteDB.Types.Header
import           Conso.Fr.Elec.SiteDB.Types.R63    (FluxR63(..), MesureR63(..))
import           Conso.Fr.Elec.SiteDB.Types.R64    (FluxR64(..), MesureR64(..))
import           Conso.Fr.Elec.SiteDB.Types.R65    (FluxR65(..), MesureR65(..))
import           Conso.Fr.Elec.SiteDB.Types.R66    (FluxR66(..), MesureR66(..))
import           Conso.Fr.Elec.SiteDB.Types.R67    (FluxR67(..), MesureR67(..))
import           Conso.Fr.Elec.SiteDB.Types.C68
import           Conso.Fr.Elec.SiteDB.Ingestion.Parser
import           Conso.Fr.Elec.SiteDB.Ingestion.Versioning ()
import           Conso.Fr.Elec.SiteDB.Storage.Insert

-- | Résultat de l'ingestion pour un PRM.
data IngestResult
  = IngestOk   PrmId Text -- ^ Succès : PRM ingéré + code flux (ex. @\"R63\"@)
  | IngestSkip PrmId Text -- ^ Ignoré : fichier déjà présent dans elec_ingestion_log
  | IngestErr  PrmId Text -- ^ Échec  : PRM + message d'erreur
  deriving (Show)

-- | Ingère un fichier JSON SGE dans les bases SQLite des PRM qu'il contient.
--
-- @openConn@ est fourni par l'appelant pour résoudre PRM → connexion SQLite.
-- La log d'ingestion et toutes les insertions métier se font dans une transaction
-- unique par PRM. Retourne un 'IngestResult' par PRM présent dans le flux.
ingestFile
  :: (PrmId -> IO Connection) -- ^ Résolution PRM → connexion (via registre UUID)
  -> CodeFlux                 -- ^ Type de flux à ingérer
  -> Maybe Text               -- ^ Nom du fichier source (pour traçabilité)
  -> ByteString               -- ^ Contenu JSON du fichier
  -> IO [IngestResult]
ingestFile openConn cf mSrc bs = do
  now <- getCurrentTime
  case parseFluxRxx cf bs of
    Left  err  -> return [IngestErr (PrmId "?") err]
    Right flux -> ingestFlux openConn cf mSrc now flux

ingestFlux :: (PrmId -> IO Connection) -> CodeFlux -> Maybe Text -> UTCTime -> FluxRxx -> IO [IngestResult]
ingestFlux openConn cf mSrc now (FluxCourbeCharge f) =
  mapM ingestM (r63Mesures f)
  where
    hdr = r63Header f
    ingestM m = do
      let prm = mr63IdPrm m
          p   = mr63Periode m
      doInsert openConn prm mSrc $ \conn ->
        logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) (hIdPublication hdr) Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        >>= \ingId -> insertCurvePoints conn ingId m
ingestFlux openConn cf mSrc now (FluxIndex f) =
  mapM ingestM (r64Mesures f)
  where
    hdr = r64Header f
    ingestM m = do
      let prm = mr64IdPrm m
          p   = mr64Periode m
      doInsert openConn prm mSrc $ \conn ->
        logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) (hIdPublication hdr) Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        >>= \ingId -> insertIndexValues conn ingId m
ingestFlux openConn cf mSrc now (FluxEnergie f) =
  mapM ingestM (r65Mesures f)
  where
    hdr = r65Header f
    ingestM m = do
      let prm = mr65IdPrm m
          p   = mr65Periode m
      doInsert openConn prm mSrc $ \conn ->
        logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) Nothing Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        >>= \ingId -> insertDailyEnergy conn ingId m
ingestFlux openConn cf mSrc now (FluxPmax f) =
  mapM ingestM (r66Mesures f)
  where
    hdr = r66Header f
    ingestM m = do
      let prm = mr66IdPrm m
          p   = mr66Periode m
      doInsert openConn prm mSrc $ \conn ->
        logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) (hIdPublication hdr) Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        >>= \ingId -> insertDailyPmax conn ingId m
ingestFlux openConn cf mSrc now (FluxFacturant f) =
  mapM ingestM (r67Mesures f)
  where
    hdr = r67Header f
    ingestM m = do
      let prm = mr67IdPrm m
          p   = mr67Periode m
      doInsert openConn prm mSrc $ \conn ->
        logIngestion conn cf
          (modePublicationToText (hModePublication hdr))
          (hIdDemande hdr) Nothing Nothing now
          (Just (periodeDebut p)) (Just (periodeFin p)) mSrc
        >>= \ingId -> insertBillingMeasures conn ingId m
ingestFlux openConn cf mSrc now (FluxITC items) =
  mapM ingestM items
  where
    ingestM itc = do
      let prm = idPrm itc
      doInsert openConn prm mSrc $ \conn ->
        logIngestion conn cf "P" "C68" Nothing Nothing now
          Nothing Nothing mSrc
        >>= \ingId -> insertPrmInfoIfChanged conn ingId now itc

-- | Ouvre la connexion via @openConn@, vérifie si le fichier a déjà été ingéré,
-- exécute l'action dans une transaction si non, ferme.
doInsert :: (PrmId -> IO Connection) -> PrmId -> Maybe Text -> (Connection -> IO ()) -> IO IngestResult
doInsert openConn prm mSrc action = do
  result <- try $ do
    conn <- openConn prm
    alreadyDone <- case mSrc of
      Nothing  -> return False
      Just src -> do
        rows <- query conn
          "SELECT COUNT(*) FROM elec_ingestion_log WHERE fichier_source = ?"
          (Only src) :: IO [Only Int]
        return $ case rows of { [Only n] -> n > 0; _ -> False }
    if alreadyDone
      then close conn >> return (IngestSkip prm (fromMaybe "" mSrc))
      else do
        withTransaction conn (action conn)
        close conn
        return (IngestOk prm (fromMaybe "" mSrc))
  return $ case result of
    Left  ex -> IngestErr  prm (T.pack (show (ex :: SomeException)))
    Right r  -> r

-- | Ingère un lot de fichiers JSON de façon séquentielle.
-- Les erreurs sur un fichier n'interrompent pas les suivants.
ingestBatch
  :: (PrmId -> IO Connection)             -- ^ Résolution PRM → connexion
  -> [(CodeFlux, Maybe Text, ByteString)] -- ^ Liste de (codeFlux, nom fichier, contenu)
  -> IO [IngestResult]
ingestBatch openConn files = do
  results <- mapM (\(cf, src, bs) -> ingestFile openConn cf src bs) files
  return (concat results)

modePublicationToText :: ModePublication -> Text
modePublicationToText MpPonctuel    = "P"
modePublicationToText MpQuotidien   = "Q"
modePublicationToText MpHebdomadaire = "H"
modePublicationToText MpMensuel     = "M"
