{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Connection
Description : Connexion SQLite d'un site avec schéma élec appliqué

Ouvre la base SQLite d'un site et applique le schéma élec (migrations).
Délègue l'ouverture physique et les PRAGMA à "Conso.Fr.SiteDB.Storage.Connection".
-}
module Conso.Fr.Elec.SiteDB.Storage.Connection
  ( openSiteDbElec
  , siteDbPath
  , configurePragmas
  ) where

import           Database.SQLite.Simple
import           Conso.Fr.SiteDB.Types                        (SiteId)
import           Conso.Fr.SiteDB.Storage.Connection           (openSiteDb, configurePragmas, siteDbPath)
import           Conso.Fr.Elec.SiteDB.Storage.Schema          (ensureSchema)

-- | Ouvre (ou crée) la base SQLite d'un site avec le schéma élec.
-- Délègue à 'openSiteDb' (core) puis applique 'ensureSchema' (migrations élec).
openSiteDbElec :: FilePath -> SiteId -> IO Connection
openSiteDbElec siteDbDir siteId = do
  conn <- openSiteDb siteDbDir siteId
  ensureSchema conn
  return conn
