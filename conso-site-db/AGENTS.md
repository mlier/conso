# File objective

This file provides guidance for coding systems when working with code in this repository.

## Build Commands

```bash
cabal build          # Build the library
cabal repl           # Interactive REPL
```

No test suite or executable is defined yet — only the library stanza exists.

## Architecture

**conso-site-db** is a Haskell library providing a central registry for French energy consumption sites. It maps a canonical **UUID** (`SiteId`) to electricity meter points (**PRM** — Enedis) and gas meter points (**PCE** — GRDF), plus an optional label.

### Two-tier storage model

- **Central registry** (`registry.db`) — one SQLite database mapping identifiers (PRM/PCE) to UUIDs. Managed by `Conso.Fr.SiteDB.Registry`.
- **Per-site databases** (`{uuid}.db`) — individual SQLite files for meter-specific data. The `Conso.Fr.SiteDB.Storage.Connection` module handles connection setup only; schema migrations are left to extensions (e.g. `conso-site-db-elec`, `conso-site-db-gaz`).

### Module responsibilities

| Module | Role |
|---|---|
| `Conso.Fr.SiteDB.Types` | Core newtypes: `SiteId`, `Prm`, `Pce`, `SiteLabel`, `SiteRef` |
| `Conso.Fr.SiteDB.Registry` | Public API — `openRegistry`, `withRegistry`, re-exports operations |
| `Conso.Fr.SiteDB.Registry.Schema` | Schema versioning and forward-only migrations (never edit published migrations) |
| `Conso.Fr.SiteDB.Registry.Operations` | CRUD: `createSite`, `lookupByPrm/Pce`, `lookupOrCreateByPrm/Pce`, `linkPrm/Pce`, `listSites` |
| `Conso.Fr.SiteDB.Storage.Connection` | Low-level connection for per-site DBs; sets WAL, synchronous=NORMAL, foreign_keys, busy_timeout=5000 |

### Key design rules

- Schema migrations in `Registry.Schema` are append-only — never modify a published migration.
- `lookupOrCreate*` operations are the idempotent entry points for callers that don't know if a site already exists.
- `withRegistry` is preferred over `openRegistry` for resource safety.
- Extension packages apply their own schema to per-site databases via `openSiteDb`.
