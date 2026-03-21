# SgeDB — Stockage et analyse des données Enedis SGE

Module Haskell de persistance et d'analyse des flux de mesures Enedis (M023).
Il reçoit des fichiers JSON issus des webservices SGE, les stocke dans des bases
SQLite (une par PRM) et expose des fonctions de requêtage, d'agrégation, de
détection de trous et d'export.

## Place dans le pipeline

```
[Webservices SGE]  →  [rfiles : récupération / décryption]  →  SgeDB  →  SQLite
```

---

## Architecture des modules

```
SgeDB.hs                 — façade publique (re-exports des sous-modules)
SgeDB/
├── Types/
│   ├── Common.hs        — PrmId, GrandeurMetier, Pas, Periode, …
│   ├── Header.hs        — CodeFlux (11 constructeurs), Header, Echeances
│   ├── R63.hs           — FluxR63, MesureR63, GrandeurR63, PointCourbe
│   ├── R64.hs           — FluxR64, MesureR64, ClasseTemporelle, Calendrier
│   ├── R65.hs           — FluxR65, MesureR65, PointEnergie
│   ├── R66.hs           — FluxR66, MesureR66, PointPmax
│   ├── R67.hs           — FluxR67, MesureR67, Quantite
│   └── C68.hs           — InfoTechniqueContractuelle (raw_json + champs clés)
├── Storage/
│   ├── Connection.hs    — prmDbPath, openPrmDb, configurePragmas
│   ├── Migration.hs     — migrations numérotées, currentSchemaVersion
│   ├── Schema.hs        — point d'entrée schéma (re-export Migration)
│   ├── Insert.hs        — insertCurvePoints / IndexValues / DailyEnergy / …
│   ├── Query.hs         — queryCurvePoints / IndexValues / DailyEnergy / …
│   └── Gaps.hs          — detectCurveGaps, detectEnergyGaps, detectPmaxGaps
├── Ingestion/
│   ├── Parser.hs        — FluxRxx, parseFluxRxx
│   ├── Versioning.hs    — getLastIngestion, IngestionInfo
│   └── Batch.hs         — ingestFile, ingestBatch, IngestResult
├── Analysis/
│   ├── Aggregate.hs     — aggregateCurve, aggregateEnergy, AggregationPeriod
│   ├── Compare.hs       — comparePeriods, ComparisonResult
│   └── Anomaly.hs       — detectAnomalies, AnomalyType, Anomaly
└── Export/
    ├── CSV.hs           — exportCurveCSV / EnergyCSV / PmaxCSV / BillingCSV
    ├── JSON.hs          — exportCurveJSON / EnergyJSON / PrmInfoJSON
    └── Consolidate.hs   — consolidateAllPrm, attachAndQuery, listAllPrmDbs
```

---

## Concepts clés

**`PrmId`** — newtype sur `Text`, identifiant à 14 chiffres d'un point de
mesure (PRM).

**`CodeFlux`** — 11 constructeurs représentant les types de fichiers M023 :
`CF_R63`, `CF_R63A`, `CF_R63B`, `CF_R64`, `CF_R64A`, `CF_R64B`, `CF_R65`,
`CF_R66`, `CF_R66B`, `CF_R67`, `CF_C68`.

**`FluxRxx`** — type union produit par le parser :

```
FluxCourbeCharge FluxR63   — courbes de charge (R63, R63A, R63B)
FluxIndex        FluxR64   — index (R64, R64A, R64B)
FluxEnergie      FluxR65   — énergies quotidiennes
FluxPmax         FluxR66   — puissances maximales (R66, R66B)
FluxFacturant    FluxR67   — mesures facturantes
FluxITC          [InfoTechniqueContractuelle]   — infos techniques (C68)
```

**Sharding** — les bases SQLite sont réparties en arborescence 3×3 chiffres
pour éviter les répertoires trop peuplés :

```
baseDir/123/456/789/12345678901234.db
```

**`ingestion_log`** — chaque appel à `ingestFile` crée une ligne de
traçabilité (code flux, mode publication, identifiant de demande, période,
fichier source).

**Migrations** — le schéma est versionné dans `Migration.hs`. La table
`schema_version` est créée au premier `openPrmDb` et les migrations
manquantes sont appliquées automatiquement dans une transaction.

---

## Schéma SQLite

Sept tables par base PRM :

| Table | Contrainte UNIQUE | Index secondaires |
|---|---|---|
| `ingestion_log` | — | `(code_flux, date_ingestion)` |
| `curve_points` | `(etape_metier, grandeur_metier, grandeur_physique, horodate, pas)` | `horodate` ; `(grandeur_metier, grandeur_physique, horodate)` |
| `index_values` | — | `horodate` ; `(contexte_releve, type_releve, horodate)` ; `(grandeur_metier, grandeur_physique, horodate)` |
| `daily_energy` | `(grandeur_metier, grandeur_physique, date_mesure)` | `date_mesure` |
| `daily_pmax` | `(grandeur_metier, grandeur_physique, horodate)` | `horodate` |
| `billing_measures` | — | `(dbt_mesure, fin_mesure)` ; `(grandeur_metier, grandeur_physique)` |
| `prm_info` | — | — (`ORDER BY id DESC` pour la dernière entrée) |

`curve_points`, `daily_energy` et `daily_pmax` utilisent `INSERT OR REPLACE`
(idempotent). `billing_measures` utilise `INSERT` simple pour conserver
l'historique complet des statuts de relève.

---

## API publique

### Ingestion

```haskell
-- Parser : JSON brut → type Haskell typé
parseFluxRxx :: CodeFlux -> ByteString -> Either Text FluxRxx

-- Ingestion d'un fichier unique
ingestFile
  :: FilePath       -- répertoire des bases SQLite
  -> CodeFlux
  -> Maybe Text     -- nom du fichier source (pour ingestion_log)
  -> ByteString     -- contenu JSON
  -> IO [IngestResult]

-- Ingestion d'un lot de fichiers
ingestBatch
  :: FilePath
  -> [(CodeFlux, Maybe Text, ByteString)]
  -> IO [IngestResult]
```

`IngestResult` vaut `IngestOk PrmId Text` ou `IngestErr PrmId Text`.
Un résultat est produit par PRM présent dans le flux.

### Connexion

```haskell
openPrmDb :: FilePath -> PrmId -> IO Connection
-- Crée les répertoires, ouvre (ou crée) la base, configure les PRAGMA
-- et applique les migrations manquantes.

prmDbPath :: FilePath -> PrmId -> FilePath
-- Calcule le chemin sans ouvrir la connexion.
```

### Requêtes

Les filtres `Maybe Text` sont optionnels — `Nothing` signifie "toutes valeurs".
Les bornes temporelles sont des chaînes ISO 8601.

```haskell
queryCurvePoints
  :: Connection
  -> Maybe Text   -- etape_metier  (ex: "BRUT", "BEST")
  -> Maybe Text   -- grandeur_metier (ex: "CONS", "PROD")
  -> Maybe Text   -- grandeur_physique (ex: "PA", "PRI")
  -> Text         -- horodate début
  -> Text         -- horodate fin
  -> IO [CurveRow]

queryIndexValues
  :: Connection
  -> Maybe Text   -- contexte_releve
  -> Maybe Text   -- grandeur_physique
  -> Text -> Text -- période
  -> IO [IndexRow]

queryDailyEnergy
  :: Connection
  -> Maybe Text   -- grandeur_metier
  -> Text -> Text -- date début/fin (YYYY-MM-DD)
  -> IO [EnergyRow]

queryDailyPmax
  :: Connection
  -> Maybe Text   -- grandeur_metier
  -> Text -> Text
  -> IO [PmaxRow]

queryBillingMeasures
  :: Connection
  -> Maybe Text   -- grandeur_metier
  -> Text -> Text -- date début/fin (YYYY-MM-DD)
  -> IO [BillingRow]

queryPrmInfo :: Connection -> IO (Maybe PrmInfoRow)
-- Retourne la dernière ligne ingérée de prm_info.
```

### Détection de trous

```haskell
detectCurveGaps
  :: Connection
  -> Text     -- grandeur_metier
  -> Text     -- grandeur_physique
  -> Text     -- etape_metier
  -> Pas      -- pas attendu (P10M, P30M, P60M, …)
  -> UTCTime  -- début de période
  -> UTCTime  -- fin de période
  -> IO [Periode]
-- Retourne les intervalles manquants sous forme de Periode (debut, fin).

detectEnergyGaps :: Connection -> Text -> Day -> Day -> IO [Day]
-- Retourne les dates manquantes dans daily_energy.

detectPmaxGaps :: Connection -> Text -> Day -> Day -> IO [Day]
-- Retourne les dates manquantes dans daily_pmax.
```

### Analyse

```haskell
aggregateCurve
  :: Connection
  -> Text              -- grandeur_metier
  -> Text              -- grandeur_physique
  -> Text              -- etape_metier
  -> AggregationPeriod -- ParJour | ParSemaine | ParMois | ParAn
  -> Text -> Text      -- période
  -> IO [AggregateRow]
-- Chaque AggregateRow contient : periode, somme, moyenne, max, nb_points.

aggregateEnergy
  :: Connection -> Text -> Text -> AggregationPeriod -> Text -> Text
  -> IO [AggregateRow]

comparePeriods
  :: Connection
  -> Text -> Text -> Text       -- grandeur_metier, grandeur_physique, etape_metier
  -> (Text, Text)               -- période de référence (début, fin)
  -> (Text, Text)               -- période de comparaison
  -> IO ComparisonResult
-- ComparisonResult : sommes, moyennes, max des deux périodes + variation en %.

detectAnomalies
  :: Connection
  -> Text -> Text -> Text       -- grandeur_metier, grandeur_physique, etape_metier
  -> Text -> Text               -- période
  -> Double                     -- seuil z-score (ex: 3.0)
  -> Maybe Double               -- borne minimale absolue
  -> Maybe Double               -- borne maximale absolue
  -> IO [Anomaly]
-- Chaque Anomaly contient : horodate, valeur, AnomalyType (ZScore | BorneMin | BorneMax).
```

### Export

```haskell
-- CSV (séparateur ";", retour immédiat en Text)
exportCurveCSV
  :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Text -> IO Text
exportEnergyCSV :: Connection -> Maybe Text -> Text -> Text -> IO Text
exportPmaxCSV   :: Connection -> Maybe Text -> Text -> Text -> IO Text
exportBillingCSV :: Connection -> Maybe Text -> Text -> Text -> IO Text

-- JSON (retourne un Value Aeson)
exportCurveJSON
  :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Text -> IO Value
exportEnergyJSON  :: Connection -> Maybe Text -> Text -> Text -> IO Value
exportPrmInfoJSON :: Connection -> IO (Maybe Value)
-- exportPrmInfoJSON re-parse le raw_json stocké et retourne le Value complet.

-- Consolidation multi-PRM
consolidateAllPrm
  :: FilePath                 -- répertoire des bases
  -> (Connection -> IO [a])   -- extracteur par connexion PRM
  -> IO [(PrmId, [a])]

attachAndQuery
  :: FromRow r
  => FilePath -> [PrmId] -> Query -> IO [r]
-- Attache jusqu'à ~125 bases via ATTACH DATABASE et exécute une requête unique.

listAllPrmDbs :: FilePath -> IO [(PrmId, FilePath)]
-- Liste tous les fichiers .db dans l'arborescence shardée.
```

---

## Exemple minimal

```haskell
import qualified Data.ByteString as BS
import Conso.Fr.Elec.SgeDB

main :: IO ()
main = do
  -- 1. Ingérer un fichier JSON R63
  bs <- BS.readFile "flux_r63.json"
  results <- ingestFile "/data/sgedb" CF_R63 (Just "flux_r63.json") bs
  mapM_ print results

  -- 2. Requêter les courbes de charge
  conn <- openPrmDb "/data/sgedb" (PrmId "12345678901234")
  rows <- queryCurvePoints conn
            (Just "CONS") (Just "PA") (Just "BRUT")
            "2024-01-01T00:00:00"
            "2024-01-31T23:59:59"
  mapM_ print rows

  -- 3. Détecter les trous sur janvier 2024 (pas 30 min)
  gaps <- detectCurveGaps conn "CONS" "PA" "BRUT" P30M
            (read "2024-01-01 00:00:00 UTC")
            (read "2024-01-31 23:59:59 UTC")
  print gaps

  -- 4. Export CSV
  csv <- exportCurveCSV conn (Just "CONS") (Just "PA") Nothing
           "2024-01-01T00:00:00" "2024-01-31T23:59:59"
  writeFile "courbes.csv" (Data.Text.unpack csv)
```

---

## Notes

- **Filtres dans `Query.hs`** — `whereClause` construit les clauses `AND`
  par interpolation de texte. Les valeurs proviennent de types Haskell
  (pas de saisie utilisateur directe), mais toute évolution vers des entrées
  libres devra migrer vers des paramètres liés (`?`).

- **C68 / `prm_info`** — seuls quelques champs structurés sont extraits
  (segment, état contractuel, alimentation, puissance souscrite, domaine
  tension). L'intégralité du JSON est conservée dans `raw_json` et
  accessible via `exportPrmInfoJSON`.

- **`billing_measures`** — pas de contrainte UNIQUE : plusieurs relevés
  avec des statuts différents peuvent coexister pour la même période.
  Utiliser `queryBillingMeasures` avec un filtre sur `grandeur_metier`
  pour distinguer les lignes.

- **Consolidation** — `attachAndQuery` utilise `ATTACH DATABASE` de SQLite,
  limité à ~125 bases simultanées. Pour des volumes plus importants,
  préférer `consolidateAllPrm` qui ouvre les connexions une à une.
