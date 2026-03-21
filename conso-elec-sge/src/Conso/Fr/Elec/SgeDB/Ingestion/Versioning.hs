{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Ingestion.Versioning
  ( getLastIngestion
  , IngestionId
  , IngestionInfo(..)
  ) where

import           Database.SQLite.Simple
import           Data.Text              (Text)
import           Conso.Fr.Elec.SgeDB.Types.Header (CodeFlux, codeFluxToText)
import           Conso.Fr.Elec.SgeDB.Storage.Insert (IngestionId)

data IngestionInfo = IngestionInfo
  { ingestId           :: IngestionId
  , ingestCodeFlux     :: Text
  , ingestModePublication :: Text
  , ingestIdDemande    :: Text
  , ingestDateIngestion :: Text
  } deriving (Eq, Show)

instance FromRow IngestionInfo where
  fromRow = IngestionInfo <$> field <*> field <*> field <*> field <*> field

-- | Retourne la dernière ingestion pour un code flux donné.
getLastIngestion :: Connection -> CodeFlux -> IO (Maybe IngestionInfo)
getLastIngestion conn cf = do
  rows <- query conn
    "SELECT id, code_flux, mode_publication, id_demande, date_ingestion \
    \ FROM ingestion_log WHERE code_flux = ? ORDER BY id DESC LIMIT 1"
    (Only (codeFluxToText cf))
  return $ case rows of
    []    -> Nothing
    (r:_) -> Just r
