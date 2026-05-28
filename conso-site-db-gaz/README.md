# conso-site-db-gaz

Bibliothèque Haskell : stockage et ingestion des données gaz GRDF (consommations publiées, informatives, injections, données contractuelles et techniques) dans des bases SQLite par PCE.

Aucun exécutable — consommée par `conso-registre`.

## Build

```bash
cabal build conso-site-db-gaz
```

## Données stockées

| Catégorie | Table | Mode insertion |
|-----------|-------|---------------|
| Consommations publiées | `gaz_conso` | INSERT OR REPLACE (idempotent) |
| Consommations informatives | `gaz_conso_informative` | INSERT OR REPLACE (idempotent) |
| Injections publiées | `gaz_injection` | INSERT OR REPLACE (idempotent) |
| Informations contractuelles | `gaz_info_contractuelle` | INSERT simple (historique) |
| Informations techniques | `gaz_info_technique` | INSERT simple (historique) |

Toutes les tables sont préfixées `gaz_` pour coexister avec les tables `elec_` dans la même base SQLite par site.

## Architecture

```
GRDF ADICT API (5 endpoints)
    ↓
conso-gaz-adict (types JSON)
    ↓
Ingestion/FromApi.hs (conversion + insertion)
    ↓
Storage/Insert.hs (SQL paramétré, transactions)
    ↓
SQLite {uuid}.db (tables gaz_*)
```

## Modules exposés

| Module | Rôle |
|--------|------|
| `SiteDB.Types` | `GazConso`, `GazInjection`, `GazInfosContractuelles`, `GazInfosTechniques` |
| `Storage.Connection` | `openSiteDbGaz` |
| `Storage.Insert` | `insertGazConsos`, `insertGazInjections`, `insertGazInfosContractuelles`, … |
| `Storage.Query` | `derniereIngestDate`, `derniereInfosContractuelles`, `detectionTrousContinu` |
| `Storage.Delete` | `deleteGazData` |
| `Ingestion.FromApi` | `ingestFromAdict` (pipeline complet 5 endpoints) |
| `Orchestration.Adresse` | `codePostalPce` |
| `Orchestration.Inscription` | `inscrirePce` |
| `Orchestration.Ingerer` | `ingererGaz` |
| `Cli` | `GazCommand`, `runGazCommand` |

## Fonctions publiques clés

### Connexion

```haskell
openSiteDbGaz :: FilePath -> SiteId -> IO Connection
-- Ouvre (ou crée) la base SQLite d'un site et applique le schéma gaz
```

### Ingestion depuis l'API ADICT

```haskell
ingestFromAdict :: AdictSession -> FilePath -> FilePath -> Pce -> Text -> Text
                -> IO AdictIngestReport
-- Pipeline complet : 5 appels API → conversion → insertion SQLite
-- Retourne un rapport (nb lignes par endpoint, changements détectés)
```

### Requêtes

```haskell
derniereIngestDate :: Connection -> Text -> IO (Maybe Text)
-- MAX(date_fin) pour un endpoint — point de reprise lors de la prochaine ingestion

derniereInfosContractuelles :: Connection -> IO (Maybe GazInfosContractuelles)
derniereInfosTechniques     :: Connection -> IO (Maybe GazInfosTechniques)

detectionTrousContinu :: Connection -> Text -> Text -> Text -> IO [(Text, Text)]
-- Lacunes de continuité dans une table (fin ≠ debut_suivant)
```

### Adresse

```haskell
codePostalPce :: AdictSession -> GetCodePostal
-- Interroge ADICT DonneesTechniques pour extraire le code postal du compteur
```

## Schéma SQLite

```sql
-- Traçabilité (1 ligne par appel API)
CREATE TABLE gaz_ingestion_log (
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  endpoint       TEXT NOT NULL,
  date_debut     TEXT, date_fin TEXT,
  periode        TEXT, type_donnee TEXT,
  date_ingestion TEXT NOT NULL,
  nb_lignes      INTEGER
);

-- Consommations publiées (idempotent)
CREATE TABLE gaz_conso (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  ingestion_id INTEGER REFERENCES gaz_ingestion_log(id),
  debut        TEXT NOT NULL, fin TEXT NOT NULL,
  -- ~25 colonnes énergétiques (énergie kWh, volumes, PCS, PTA, indices, qualité…)
  UNIQUE(debut, fin)
);
-- Même structure pour gaz_conso_informative et gaz_injection

-- Historique complet (INSERT simple, conserve toutes les versions)
CREATE TABLE gaz_info_contractuelle (
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  ingestion_id   INTEGER REFERENCES gaz_ingestion_log(id),
  date_ingestion TEXT NOT NULL
  -- CAR, CJA, tarifs, profils, modulations…
);
CREATE TABLE gaz_info_technique (
  -- Adresse, calibre compteur, PITD, régimes propriété…
);
```

## Comportement d'ingestion

- **Consommations / injections** : `INSERT OR REPLACE` sur `UNIQUE(debut, fin)` — réingestion sans duplication.
- **Infos contractuelles/techniques** : insérées uniquement si un champ a changé par rapport à la dernière valeur connue — historique complet préservé.
- **Granularité** (journalier vs mensuel) : inférée automatiquement de la longueur de la plage de dates, sans champ explicite dans l'API GRDF.
- **Détection de lacunes** : `detectionTrousContinu` identifie les trous dans les séries temporelles pour déclencher un backfill.
