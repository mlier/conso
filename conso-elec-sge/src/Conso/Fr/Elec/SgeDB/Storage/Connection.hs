{-# LANGUAGE OverloadedStrings #-}
module Conso.Fr.Elec.SgeDB.Storage.Connection
  ( prmDbPath
  , openPrmDb
  , configurePragmas
  ) where

import           Database.SQLite.Simple
import           System.FilePath        ((</>), (<.>), takeDirectory)
import           System.Directory       (createDirectoryIfMissing)
import qualified Data.Text              as T
import           Conso.Fr.Elec.SgeDB.Types.Common  (PrmId(..))
import           Conso.Fr.Elec.SgeDB.Storage.Schema (ensureSchema)

-- | Calcule le chemin SQLite d'un PRM avec sharding 3 niveaux × 3 chiffres.
-- Exemple : PrmId "12345678901234"
--   → baseDir/123/456/789/12345678901234.db
prmDbPath :: FilePath -> PrmId -> FilePath
prmDbPath baseDir (PrmId prm) =
  baseDir
    </> T.unpack (T.take 3 prm)
    </> T.unpack (T.take 3 (T.drop 3 prm))
    </> T.unpack (T.take 3 (T.drop 6 prm))
    </> T.unpack prm <.> "db"

-- | Ouvre (ou crée) la base SQLite d'un PRM.
-- Crée automatiquement les sous-répertoires, applique les PRAGMA et les migrations.
openPrmDb :: FilePath -> PrmId -> IO Connection
openPrmDb baseDir prmId = do
  let dbPath = prmDbPath baseDir prmId
  createDirectoryIfMissing True (takeDirectory dbPath)
  conn <- open dbPath
  configurePragmas conn
  ensureSchema conn
  return conn

-- | Configure les PRAGMA SQLite pour chaque connexion ouverte.
configurePragmas :: Connection -> IO ()
configurePragmas conn = do
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA synchronous = NORMAL"
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA busy_timeout = 5000"
