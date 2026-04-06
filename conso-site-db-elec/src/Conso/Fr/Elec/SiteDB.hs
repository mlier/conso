{-|
Module      : Conso.Fr.Elec.SiteDB
Description : Façade publique du stockage et analyse des données Enedis SGE

Point d'entrée unique du pipeline SgeDB. Re-exporte les types, fonctions
et connexions pour :

  1. __Parser__ un fichier JSON Enedis (flux M023 : R63, R64, R65, R66, R67, C68)
  2. __Stocker__ les mesures dans une base SQLite par site (nommée par UUID)
  3. __Requêter__ les données par période, grandeur et étape métier

La connexion à la base d'un site s'ouvre via 'openSiteDbElec' après résolution
de l'UUID depuis le registre central ("Conso.Fr.SiteDB.Registry").
-}
module Conso.Fr.Elec.SiteDB
  ( -- * Types communs
    module Conso.Fr.Elec.SiteDB.Types.Common
    -- * Types d'en-tête et code flux
  , module Conso.Fr.Elec.SiteDB.Types.Header
    -- * Parser et type union
  , module Conso.Fr.Elec.SiteDB.Ingestion.Parser
    -- * Connexion SQLite site
  , module Conso.Fr.Elec.SiteDB.Storage.Connection
    -- * Requêtes
  , module Conso.Fr.Elec.SiteDB.Storage.Query
    -- * Ingestion par batch
  , module Conso.Fr.Elec.SiteDB.Ingestion.Batch
  ) where

import Conso.Fr.Elec.SiteDB.Types.Common
import Conso.Fr.Elec.SiteDB.Types.Header
import Conso.Fr.Elec.SiteDB.Ingestion.Parser
import Conso.Fr.Elec.SiteDB.Storage.Connection
import Conso.Fr.Elec.SiteDB.Storage.Query
import Conso.Fr.Elec.SiteDB.Ingestion.Batch
