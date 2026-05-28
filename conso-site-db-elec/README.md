# conso-site-db-elec

Bibliothèque Haskell : stockage, ingestion, analyse et export des données électricité Enedis
(flux M023/R6X/C68/NASS) dans des bases SQLite par PRM.

Aucun exécutable — consommée par `conso-registre`.

## Build

```bash
cabal build conso-site-db-elec
```

## Flux supportés

| Code | Données | Table SQLite |
|------|---------|--------------|
| R63 / R63A / R63B | Courbes de charge (PA, PRI, PRC, E) | `elec_curve_points` |
| R64 / R64A / R64B | Index compteur | `elec_index_values` |
| R65 | Énergies quotidiennes | `elec_daily_energy` |
| R66 / R66B | Puissances maximales (Pmax) | `elec_daily_pmax` |
| R67 | Mesures facturantes | `elec_billing_measures` |
| C68 | Informations techniques et contractuelles | `elec_prm_info` + 3 tables |
| NASS | Arrêts de services souscrits | `elec_service_arrets` |

## Architecture en couches

```
Types (Common, Header, R63–R67, C68, Nass)
    ↓
Storage (Connection, Migration, Insert, Query, Gaps, Delete)
    ↓
Ingestion (Parser, Batch, FromRfiles, GapFill)
    ↓
Analysis (Aggregate, Compare, Anomaly) / Export (CSV, JSON)
    ↓
Orchestration (Adresse, Inscription, Backfill, Ingerer) / Cli
    ↓
SiteDB (façade publique)
```

## Fonctions publiques clés

### Connexion

```haskell
openSiteDbElec :: FilePath -> SiteId -> IO Connection
-- Ouvre (ou crée) la base SQLite d'un site et applique le schéma
```

### Parsing

```haskell
parseFluxRxx :: CodeFlux -> ByteString -> Either Text FluxRxx
-- Point d'entrée unique pour parser un fichier JSON Enedis
```

### Ingestion

```haskell
ingestFile :: (PrmId -> IO Connection) -> CodeFlux -> Maybe Text -> ByteString -> IO [IngestResult]
-- Ingère un fichier ; un IngestResult (Ok/Skip/Err) par PRM

ingestBatch :: (PrmId -> IO Connection) -> [(CodeFlux, Maybe Text, ByteString)] -> IO [IngestResult]
-- Ingère un lot ; transactions isolées par PRM

ingestDirectory :: FilePath -> FilePath -> FilePath -> IO [IngestDirResult]
-- Ingère tous les .json d'un répertoire (détecte CodeFlux automatiquement)
```

### Requêtes

```haskell
queryCurvePoints     :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO [CurveRow]
queryIndexValues     :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO [IndexRow]
queryDailyEnergy     :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO [EnergyRow]
queryDailyPmax       :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO [PmaxRow]
queryBillingMeasures :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO [BillingRow]
queryLatestPrmInfo   :: Connection -> IO (Maybe PrmInfoRow)
queryServiceArrets   :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> IO [ServiceArretRow]
```

### Adresse

```haskell
codePostalPrm :: Bool -> Bool -> GetCodePostal
-- Interroge SGE (ConsulterDonneesTechniquesContractuellesV10) pour obtenir le code postal
```

## Schéma SQLite

Tables principales (schéma v1, immuable) :

| Table | Clé unique | Mode insertion |
|-------|-----------|---------------|
| `elec_ingestion_log` | — | INSERT |
| `elec_curve_points` | `(etape, grandeur, horodate, pas)` | INSERT OR REPLACE |
| `elec_index_values` | — | INSERT |
| `elec_daily_energy` | `(grandeur, date)` | INSERT OR REPLACE |
| `elec_daily_pmax` | `(grandeur, horodate)` | INSERT OR REPLACE |
| `elec_billing_measures` | — | INSERT |
| `elec_prm_info` | — | INSERT (si changement) |
| `elec_service_arrets` | `(type, segment, dates)` | INSERT OR IGNORE |
| `elec_backfill_log` | — | INSERT |

## Export

```haskell
exportCurveCSV  :: Connection -> FilePath -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
-- Séparateur ';' (norme française Excel)

exportCurveJSON :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> IO LBS.ByteString
```
