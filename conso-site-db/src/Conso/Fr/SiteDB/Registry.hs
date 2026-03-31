{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.SiteDB.Registry
Description : Façade pour l'accès au registre central des sites

Fournit une fonction d'ouverture du registre (@registry.db@) et
ré-exporte les opérations CRUD depuis "Conso.Fr.SiteDB.Registry.Operations".

Usage typique :

@
import Conso.Fr.SiteDB.Registry

main :: IO ()
main = withRegistry "\/home\/user\/.conso" $ \\conn -> do
  siteId <- lookupOrCreateByPrm conn (Prm "12345678901234")
  print siteId
@
-}
module Conso.Fr.SiteDB.Registry
  ( -- * Ouverture du registre
    openRegistry
  , withRegistry
    -- * Opérations CRUD (ré-exports)
  , module Conso.Fr.SiteDB.Registry.Operations
  ) where

import           Database.SQLite.Simple
import           System.FilePath                    ((</>))
import           System.Directory                   (createDirectoryIfMissing)
import           Conso.Fr.SiteDB.Registry.Schema      (ensureRegistrySchema)
import           Conso.Fr.SiteDB.Registry.Operations

-- | Chemin du fichier registre dans le répertoire de configuration.
registryDbPath :: FilePath -> FilePath
registryDbPath configDir = configDir </> "registry.db"

-- | Ouvre (ou crée) le registre central.
-- Crée le répertoire @configDir@ si absent, applique les migrations.
openRegistry :: FilePath -> IO Connection
openRegistry configDir = do
  createDirectoryIfMissing True configDir
  conn <- open (registryDbPath configDir)
  ensureRegistrySchema conn
  return conn

-- | Ouvre le registre, exécute une action, puis ferme la connexion.
withRegistry :: FilePath -> (Connection -> IO a) -> IO a
withRegistry configDir action = do
  conn <- openRegistry configDir
  result <- action conn
  close conn
  return result
