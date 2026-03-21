{-|
Module      : Conso.Fr.Elec.SgeDB
Description : Façade publique du module de stockage et d'analyse des données Enedis SGE

Ce module est le point d'entrée unique du pipeline SgeDB. Il re-exporte
les types, fonctions et connexions nécessaires pour :

  1. __Parser__ un fichier JSON Enedis (flux M023 : R63, R64, R65, R66, R67, C68)
  2. __Stocker__ les mesures dans une base SQLite par PRM (sharding 3×3 chiffres)
  3. __Requêter__ les données par période, grandeur et étape métier

Pour les fonctionnalités avancées (agrégation, comparaison, détection d'anomalies,
export CSV/JSON, consolidation multi-PRM), importer les sous-modules directement :

  * "Conso.Fr.Elec.SgeDB.Analysis.Aggregate"
  * "Conso.Fr.Elec.SgeDB.Analysis.Compare"
  * "Conso.Fr.Elec.SgeDB.Analysis.Anomaly"
  * "Conso.Fr.Elec.SgeDB.Export.CSV"
  * "Conso.Fr.Elec.SgeDB.Export.JSON"
  * "Conso.Fr.Elec.SgeDB.Export.Consolidate"
  * "Conso.Fr.Elec.SgeDB.Storage.Gaps"

Pipeline d'utilisation typique :

> import Conso.Fr.Elec.SgeDB
>
> -- 1. Ouvrir (ou créer) une base pour un PRM
> conn <- openPrmDb "/data/sgedb" (PrmId "12345678901234")
>
> -- 2. Parser et ingérer un fichier JSON
> bs <- readFile "flux_r63.json"
> results <- ingestFile "/data/sgedb" CF_R63 (Just "flux_r63.json") bs
>
> -- 3. Requêter
> rows <- queryCurvePoints conn (Just "CONS") (Just "PA") (Just "BRUT")
>           "2024-01-01T00:00:00" "2024-01-31T23:59:59"

Modules disponibles :

* "Conso.Fr.Elec.SgeDB.Types.Common"  — Types partagés (PrmId, GrandeurMetier, Pas, ...)
* "Conso.Fr.Elec.SgeDB.Types.Header"  — Header, CodeFlux, Echeances
* "Conso.Fr.Elec.SgeDB.Types.R63"     — Courbes de charge
* "Conso.Fr.Elec.SgeDB.Types.R64"     — Index
* "Conso.Fr.Elec.SgeDB.Types.R65"     — Energies quotidiennes
* "Conso.Fr.Elec.SgeDB.Types.R66"     — Pmax quotidiennes
* "Conso.Fr.Elec.SgeDB.Types.R67"     — Mesures facturantes
* "Conso.Fr.Elec.SgeDB.Types.C68"     — Informations techniques et contractuelles
* "Conso.Fr.Elec.SgeDB.Storage.Connection"  — Connexion SQLite par PRM (sharding 3×3)
* "Conso.Fr.Elec.SgeDB.Storage.Migration"   — Migrations numérotées
* "Conso.Fr.Elec.SgeDB.Storage.Schema"      — Point d'entrée schéma
* "Conso.Fr.Elec.SgeDB.Storage.Insert"      — Insertions par type de flux
* "Conso.Fr.Elec.SgeDB.Storage.Query"       — Requêtes par période/grandeur
* "Conso.Fr.Elec.SgeDB.Storage.Gaps"        — Détection de trous
* "Conso.Fr.Elec.SgeDB.Ingestion.Parser"    — Parsing JSON → FluxRxx
* "Conso.Fr.Elec.SgeDB.Ingestion.Versioning" — Gestion des publications récurrentes
* "Conso.Fr.Elec.SgeDB.Ingestion.Batch"     — Ingestion par fichier et par batch
* "Conso.Fr.Elec.SgeDB.Analysis.Aggregate"  — Agrégations temporelles
* "Conso.Fr.Elec.SgeDB.Analysis.Compare"    — Comparaison de périodes
* "Conso.Fr.Elec.SgeDB.Analysis.Anomaly"    — Détection d'anomalies
* "Conso.Fr.Elec.SgeDB.Export.CSV"          — Export CSV
* "Conso.Fr.Elec.SgeDB.Export.JSON"         — Re-sérialisation JSON
* "Conso.Fr.Elec.SgeDB.Export.Consolidate"  — Consolidation multi-PRM
-}
module Conso.Fr.Elec.SgeDB
  ( -- * Types communs
    module Conso.Fr.Elec.SgeDB.Types.Common
    -- * Types d'en-tête et code flux
  , module Conso.Fr.Elec.SgeDB.Types.Header
    -- * Parser et type union
  , module Conso.Fr.Elec.SgeDB.Ingestion.Parser
    -- * Connexion SQLite
  , module Conso.Fr.Elec.SgeDB.Storage.Connection
    -- * Requêtes
  , module Conso.Fr.Elec.SgeDB.Storage.Query
    -- * Ingestion par batch
  , module Conso.Fr.Elec.SgeDB.Ingestion.Batch
  ) where

import Conso.Fr.Elec.SgeDB.Types.Common
import Conso.Fr.Elec.SgeDB.Types.Header
import Conso.Fr.Elec.SgeDB.Ingestion.Parser
import Conso.Fr.Elec.SgeDB.Storage.Connection
import Conso.Fr.Elec.SgeDB.Storage.Query
import Conso.Fr.Elec.SgeDB.Ingestion.Batch
