# File objective

This file provides guidance to coding systems when working with code in this repository.

## Commandes courantes

```bash
# Compilation
cabal build                            # tout
cabal build lib:conso-gaz-adict        # bibliothèque seule
cabal build exe:conso-gaz-adict        # exécutable seul

# Tests
cabal test --test-show-details=direct  # tous les tests avec sortie
# Un seul module de test :
cabal test conso-gaz-adict-test --test-option=--match \
  --test-option="Conso.Fr.Gaz.Adict.ConsosPublieesSpec"

# Lancement CLI
cabal run conso-gaz-adict -- conso --pce 12345678901234 --periode 2024 --prod
cabal run conso-gaz-adict -- liste --role acf --prod
cabal run conso-gaz-adict -- declarer --pce 12345678901234 --cp 75001 --2ans \
  --nom "Dupont Jean" --contractuelles --techniques --informatives --publiees --prod
```

## Architecture

### Structure des cibles cabal

| Cible | Répertoire | Rôle |
|-------|-----------|------|
| `lib:conso-gaz-adict` | `src/` | Client GRDF ADICT (réutilisable) |
| `exe:conso-gaz-adict` | `app/` | CLI + TUI Brick |
| `test:conso-gaz-adict-test` | `test/` | Tests d'intégration sandbox |

### Bibliothèque (`src/Conso/Fr/Gaz/Adict/`)

- **`Adict.hs`** — Infrastructure centrale : `AdictSession` (OAuth2 client credentials + cache de token `IORef`, manager TLS partagé), fonctions HTTP (`adictGet`, `adictGetNDJSON`, `adictPut`, `adictPost`, `adictPatch`), type `AdictError`, lecture de `~/.conso/conso-env.yaml`.
- **`Types.hs`** — Tous les types de réponse API avec instances `FromJSON`/`ToJSON` manuelles. Préfixes de champs systématiques : `cr_` (ConsoRestit), `da_` (DroitAcces), `din_` (DemandeAccesIn), etc.
- **`Api.hs`** — Définition Servant de l'API et type de contenu `NDJSON` (`application/x-ndjson`) avec `MimeUnrender` personnalisé.
- **Un module par endpoint** (`ConsosPubliees`, `ConsosInfos`, `InjectionsPubliees`, `DonneesContractuelles`, `DonneesTechniques`, `DroitsAcces`, `DroitAcces`, `Preuves`) — chacun exporte une fonction principale et une variante `*Sandbox`.

### CLI + affichage (`app/`)

- **`Main.hs`** — Parsing optparse-applicative (10 sous-commandes), dispatch vers la bibliothèque. Les rôles courts (`acf/dcf/aci/dci`) sont expandés via `expandRole`/`roleTiersFromCli`. Les flags `--1an/--2ans/--3ans` calculent les dates automatiquement.
- **`Display.hs`** — Typeclass `Renderable` (`toWidget`, `toHeader`), `renderApp` (viewport Brick scrollable, quitter avec q/Esc/↑/↓), attributs de style (`errorAttr`, `sectionAttr`, `labelAttr`).
- **`Display/*.hs`** — Instances `Renderable` orphelines par famille de types. `ConsoDisplay` utilise des newtypes (`ConsosPubliees`, `ConsosInfos`) pour distinguer les deux instances sur `[ConsoRestit]`.

### Tests (`test/`)

- `Main.hs` : `hspec-discover` — pas de liste manuelle.
- `SpecHelper.hs` : `sandboxSession`, `wsRequestTest`, `pendingOnAdictError` (passe si le sandbox est injoignable, échoue sur les assertions).
- `TestData.hs` : PCE du JDD sandbox GRDF v1.4.
- Chaque `*Spec.hs` correspond à un module bibliothèque.

## Conventions non-évidentes

**Erreurs fonctionnelles GRDF** : L'API renvoie HTTP 200 avec un objet `statut_restitution` contenant un `code` d'erreur. `adictGet`/`adictGetNDJSON` détectent ce cas via `checkFunctionalErrorVal` et retournent un `Left (FunctionalError code msg)`.

**Réponses NDJSON mixtes** : Le flux peut contenir des objets GDA (`code_statut_traitement`) intercalés avec les objets de données. `parseNDJSON` filtre ces objets via la clé `code_statut_traitement` avant de décoder.

**Configuration** : `~/.conso/conso-env.yaml` contient les credentials OAuth2 pour sandbox et production. `initSession prod debug verbose` choisit l'environnement.

**Cellules Brick à largeur fixe** : `tableCell` dans `ConsoDisplay.hs` calcule le padding côté Haskell (pas via `padLeft/padRight Max`) pour garantir une largeur exacte même sur les valeurs vides.

**`renderJsonSection`** dans `DonneesDisplay.hs` : affiche un `Value` Aeson de type objet sous forme de liste `clé : valeur` (utilisé pour les champs `caracteristiques_compteur`, `regime_propriete`, `car`, `cja`, `profil`, `modulation`).
