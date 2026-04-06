{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Conso.Fr.Gaz.SiteDB.Storage.Connection
Description : Connexion SQLite d'un site avec schéma gaz appliqué

Ouvre la base SQLite d'un site et applique le schéma gaz (migrations).
Délègue l'ouverture physique et les PRAGMA à "Conso.Fr.SiteDB.Storage.Connection".
-}
module Conso.Fr.Gaz.SiteDB.Storage.Connection
  ( openSiteDbGaz ) where

import Database.SQLite.Simple
import Conso.Fr.SiteDB.Types (SiteId)
import Conso.Fr.SiteDB.Storage.Connection (openSiteDb)
import Conso.Fr.Gaz.SiteDB.Storage.Migration (ensureGazSchema)

-- | Ouvre (ou crée) la base SQLite d'un site avec le schéma gaz.
-- Délègue à 'openSiteDb' (core) puis applique 'ensureGazSchema'.
openSiteDbGaz :: FilePath -> SiteId -> IO Connection
openSiteDbGaz siteDbDir siteId = do
    conn <- openSiteDb siteDbDir siteId
    ensureGazSchema conn
    return conn
