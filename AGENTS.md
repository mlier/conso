# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Monorepo structure

6 packages dans `cabal.project` ; chaque sous-dossier a son propre `AGENTS.md` avec l'architecture détaillée.

| Package | Rôle |
|---------|------|
| `conso-site-db` | Registre SQLite central PRM/PCE → UUID (bibliothèque pure, sans dépendances API) |
| `conso-site-db-elec` | Stockage + orchestration + CLI élec (bibliothèque) |
| `conso-site-db-gaz` | Stockage + orchestration + CLI gaz (bibliothèque) |
| `conso-elec-sge` | Client SOAP Enedis SGE + CLI/TUI |
| `conso-gaz-adict` | Client REST GRDF ADICT OAuth2 + CLI/TUI |
| `conso-registre` | Exécutable assembleur — compose les parsers et commandes des extensions |

Dépendances inter-packages (sens unique) :
```
conso-elec-sge          conso-gaz-adict
      ↑                       ↑
conso-site-db-elec     conso-site-db-gaz
  (Orchestration, Cli)   (Orchestration, Cli)
      ↑         ↑           ↑         ↑
      └──────── conso-site-db ─────────┘
                  (bibliothèque pure)
                    ↑
              conso-registre
              (exe: Main.hs)
```

### Principe architectural clé : `GetCodePostal`

`conso-site-db` définit `type GetCodePostal = Text -> IO (Either String Text)`. Toute extension gérant un point de livraison avec une adresse physique doit exporter une valeur de ce type :
- `conso-site-db-elec` exporte `codePostalPrm :: Bool -> Bool -> GetCodePostal`
- `conso-site-db-gaz` exporte `codePostalPce :: AdictSession -> GetCodePostal`

Le core vérifie la cohérence via `verifierCoherence`. L'injection croisée se fait dans `conso-registre` (le seul endroit qui importe les deux extensions).

## Commandes racine

```bash
cabal build        # tous les packages
cabal test         # toutes les suites de tests
cabal clean
```

## Configuration partagée

Tous les packages lisent `~/.conso/conso-env.yaml`. Les bases SQLite sont dans `~/.conso/` (`registry.db` + `{uuid}.db` par site).
