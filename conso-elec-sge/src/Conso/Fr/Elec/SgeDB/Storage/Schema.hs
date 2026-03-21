module Conso.Fr.Elec.SgeDB.Storage.Schema
  ( ensureSchema
  , currentSchemaVersion
  ) where

-- Point d'entrée unique pour le schéma : délègue à Migration.
import Conso.Fr.Elec.SgeDB.Storage.Migration (ensureSchema, currentSchemaVersion)
