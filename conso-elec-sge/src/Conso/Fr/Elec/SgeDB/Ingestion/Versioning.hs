{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Ingestion.Versioning
Description : Consultation de l'historique d'ingestion par code flux

Permet d'interroger @ingestion_log@ pour savoir si un flux a déjà été ingéré
et avec quels paramètres. Utile pour éviter la ré-ingestion de fichiers déjà traités.
-}
module Conso.Fr.Elec.SgeDB.Ingestion.Versioning
  ( getLastIngestion
  , IngestionId
  , IngestionInfo(..)
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import           Conso.Fr.Elec.SgeDB.Types.Header (CodeFlux, codeFluxToText)
import           Conso.Fr.Elec.SgeDB.Storage.Insert (IngestionId)

-- | Informations sur la dernière ingestion d'un flux pour un PRM.
data IngestionInfo = IngestionInfo
  { ingestId              :: IngestionId -- ^ Identifiant de la ligne dans @ingestion_log@
  , ingestCodeFlux        :: Text        -- ^ Code flux ingéré (ex. @\"R63\"@)
  , ingestModePublication :: Text        -- ^ Mode de publication (@\"P\"@, @\"Q\"@, …)
  , ingestIdDemande       :: Text        -- ^ Identifiant de la demande SGE
  , ingestDateIngestion   :: Text        -- ^ Horodate d'ingestion (ISO 8601)
  } deriving (Eq, Show)

instance FromRow IngestionInfo where
  fromRow = IngestionInfo <$> field <*> field <*> field <*> field <*> field

-- | Retourne la dernière ingestion enregistrée pour un code flux donné.
-- Retourne 'Nothing' si aucune ingestion n'a encore eu lieu.
getLastIngestion :: Connection -> CodeFlux -> IO (Maybe IngestionInfo)
getLastIngestion conn cf = do
  rows <- query conn
    "SELECT id, code_flux, mode_publication, id_demande, date_ingestion \
    \ FROM ingestion_log WHERE code_flux = ? ORDER BY id DESC LIMIT 1"
    (Only (codeFluxToText cf))
  return $ case rows of
    []    -> Nothing
    (r:_) -> Just r
