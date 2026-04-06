{-|
Module      : Conso.Fr.Elec.SiteDB.Storage.Schema
Description : Point d'entrée unique pour le schéma SQLite (délègue à Migration)

Ce module re-exporte 'ensureSchema' et 'currentSchemaVersion' depuis
"Conso.Fr.Elec.SiteDB.Storage.Migration". Il sert de point d'accès stable
pour les autres modules de Storage qui n'ont pas besoin d'importer Migration
directement.
-}
module Conso.Fr.Elec.SiteDB.Storage.Schema
  ( ensureSchema
  , currentSchemaVersion
  ) where

-- Point d'entrée unique pour le schéma : délègue à Migration.
import Conso.Fr.Elec.SiteDB.Storage.Migration (ensureSchema, currentSchemaVersion)
