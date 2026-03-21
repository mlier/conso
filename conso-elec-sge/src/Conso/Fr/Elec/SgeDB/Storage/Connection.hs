{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SgeDB.Storage.Connection
Description : Connexion SQLite par PRM avec sharding 3×3 chiffres

Gère l'ouverture des bases SQLite, une par PRM, dans une arborescence
répartie sur 3 niveaux de 3 chiffres pour éviter les répertoires trop peuplés :

> baseDir/123/456/789/12345678901234.db

À chaque ouverture, les PRAGMA WAL sont configurés et les migrations
manquantes sont appliquées (voir "Conso.Fr.Elec.SgeDB.Storage.Migration").
-}
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

-- | Configure les PRAGMA SQLite optimisés pour les accès concurrents.
-- PRAGMA appliqués :
--
-- * @journal_mode = WAL@ — écriture non bloquante
-- * @synchronous = NORMAL@ — compromis performance\/durabilité
-- * @foreign_keys = ON@ — intégrité référentielle activée
-- * @busy_timeout = 5000@ — attente 5 s en cas de verrou
configurePragmas :: Connection -> IO ()
configurePragmas conn = do
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA synchronous = NORMAL"
  execute_ conn "PRAGMA foreign_keys = ON"
  execute_ conn "PRAGMA busy_timeout = 5000"
