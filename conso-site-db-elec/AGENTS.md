# File objective

This file provides guidance for coding systems when working with code in this repository.

## Build commands

```bash
cabal build          # compile the library
cabal repl           # interactive REPL
cabal clean          # clean build artifacts
```

No test suite is configured in the `.cabal` file yet.

## Project purpose

Pure Haskell library for ingesting, storing, querying, and exporting French electricity (Enedis SGE) flux data from M023/R6X JSON files into per-PRM SQLite databases. No executable — consumed by other packages.

## Architecture

Layered, with strict one-way dependencies:

```
Types → Storage → Ingestion → Analysis / Export → public facade (SiteDB.hs)
```

- **Types/** — data structures for each flux: `R63` (load curves), `R64` (indexes), `R65` (daily energy), `R66` (Pmax), `R67` (billing), `C68` (technical/contractual info). `Common` holds shared enums (`GrandeurMetier`, `Pas`, `NaturePoint`, …).
- **Storage/** — SQLite layer: `Connection` (one DB per PRM UUID), `Migration` (immutable versioned migrations, current version = 1), `Insert`, `Query`, `Gaps`.
- **Ingestion/** — `Parser` dispatches on `CodeFlux` to produce `FluxRxx`; `Batch` wraps each PRM in a transaction; `FromRfiles` loads R-format files.
- **Analysis/** — `Aggregate` (by day/week/month/year), `Compare`, `Anomaly`.
- **Export/** — `CSV` (semicolon separator for French Excel), `JSON`, `Consolidate`.
- **`SiteDB.hs`** — single re-export facade; the only module consumers need to import.

## Key design rules

- Each PRM site gets its own SQLite file (path derived from UUID + base directory).
- Schema migrations are **immutable**: never modify existing migration entries, only add new ones and increment `currentSchemaVersion`.
- Batch ingestion isolates each PRM in its own transaction so one failure doesn't abort the others.
- `FluxRxx` is the union type covering all 11 `CodeFlux` variants; `parseFluxRxx` is the single entry point for parsing.
- C68 JSON structure differs from R6X (array at top level vs. object+array) — handle in parser.
- CSV exports use `;` as separator (French locale convention).

## Dependency on `conso-site-db`

This library depends on `conso-site-db` for the PRM registry / site-UUID mapping. Changes to PRM lookup or site connection logic must be coordinated with that package.
