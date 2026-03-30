{-|
Module      : Conso.Fr.Site.SiteDB.Elec.Storage.Schema
Description : Point d'entrée unique pour le schéma SQLite (délègue à Migration)

Ce module re-exporte 'ensureSchema' et 'currentSchemaVersion' depuis
"Conso.Fr.Site.SiteDB.Elec.Storage.Migration". Il sert de point d'accès stable
pour les autres modules de Storage qui n'ont pas besoin d'importer Migration
directement.
-}
module Conso.Fr.Site.SiteDB.Elec.Storage.Schema
  ( ensureSchema
  , currentSchemaVersion
  ) where

-- Point d'entrée unique pour le schéma : délègue à Migration.
import Conso.Fr.Site.SiteDB.Elec.Storage.Migration (ensureSchema, currentSchemaVersion)
