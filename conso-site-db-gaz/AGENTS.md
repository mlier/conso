# File objective

This file provides guidance for coding systems when working with code in this repository.

## Commands

All `cabal` commands must be run from the monorepo root (`../`), not this directory:

```bash
# Build this library
cabal build conso-site-db-gaz

# Interactive REPL
cabal repl conso-site-db-gaz

# Build everything (all local packages)
cabal build all
```

There is no test suite for this package.

## Architecture

**conso-site-db-gaz** is a SQLite storage layer for French gas (GRDF) consumption data. It is one package in a Haskell monorepo under `conso/`. It depends on two sibling packages:
- `conso-site-db` — core SQLite connection management and site registry (PCE → UUID → `.db` file)
- `conso-gaz-adict` — GRDF ADICT API client (HTTP calls, API types)

### Data flow

```
GRDF ADICT API
  → conso-gaz-adict (API types)
  → Ingestion/FromApi.hs (conversion)
  → SiteDB.Types (domain types)
  → Storage/Insert.hs (SQL)
  → SQLite {uuid}.db (gaz_* tables)
```

### Module responsibilities

| Module | Role |
|---|---|
| `SiteDB.Types` | Domain types for SQLite: `GazConso`, `GazInjection`, `GazInfosContractuelles`, `GazInfosTechniques`, `TypeDonnee`, `PeriodeGaz` |
| `Storage.Connection` | `openSiteDbGaz`: opens/creates a site DB and applies gas migrations |
| `Storage.Migration` | Immutable versioned DDL; `gaz_schema_version` table tracks applied versions |
| `Storage.Insert` | Idempotent `INSERT OR REPLACE` for consos/injections; plain `INSERT` for contractual/technical info (history preserved) |
| `Ingestion.FromApi` | `ingestFromAdict`: full pipeline — looks up site in registry, calls 5 ADICT endpoints, converts and inserts results, returns `AdictIngestReport` |

### Schema conventions

- All tables are prefixed `gaz_` so they coexist with electricity tables in the same `.db` file.
- `gaz_consos` and `gaz_injections` have a `UNIQUE(date_debut, date_fin, type_donnee, periode)` constraint; inserts are idempotent via `INSERT OR REPLACE`.
- `gaz_infos_contractuelles` and `gaz_infos_techniques` use plain `INSERT` to keep a full history.
- `gaz_ingestion_log` records each API call; all data rows carry a foreign key `ingestion_id`.
- Never modify existing migration SQL — add a new version instead.

### Type mapping (API → DB)

Conversion functions in `Ingestion/FromApi.hs` map GRDF ADICT types (`conso-gaz-adict`) to the local domain types (`SiteDB.Types`). Period granularity (`PJournalier`/`PMensuel`) is inferred from date-range length, not from a field in the API response.
