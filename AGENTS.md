# File objective

This file provides guidance for coding systems when working with code in this repository.

## Monorepo structure

5 packages dans `cabal.project` ; chaque sous-dossier a son propre `AGENTS.md` avec l'architecture détaillée.

| Package | Rôle |
|---------|------|
| `conso-site-db` | Registre SQLite central PRM/PCE → UUID (bibliothèque) |
| `conso-site-db-elec` | Stockage flux électricité M023/R6X (bibliothèque) |
| `conso-site-db-gaz` | Stockage données gaz GRDF (bibliothèque) |
| `conso-elec-sge` | Client SOAP Enedis SGE + CLI/TUI |
| `conso-gaz-adict` | Client REST GRDF ADICT OAuth2 + CLI/TUI |

Dépendances inter-packages (sens unique) :
- `conso-site-db` ← `conso-site-db-elec`
- `conso-site-db` + `conso-gaz-adict` ← `conso-site-db-gaz`

## Commandes racine

```bash
cabal build        # tous les packages
cabal test         # toutes les suites de tests
cabal clean
```

## Configuration partagée

Tous les packages lisent `~/.conso/conso-env.yaml`. Les bases SQLite sont dans `~/.conso/` (`registry.db` + `{uuid}.db` par site).
