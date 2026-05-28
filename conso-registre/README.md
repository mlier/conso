# Conso registre

Exécutable CLI principal du monorepo. Assemble les commandes des deux extensions (élec + gaz) en un outil unifié pour inscrire des sites, ingérer des données et gérer le registre.

## Build

```bash
cabal build exe:conso-registre
```

## Utilisation

```bash
cabal run conso-registre -- [OPTIONS GLOBALES] <COMMANDE>
```

Options globales :

| Option | Description |
|--------|-------------|
| `--config-dir DIR` | Répertoire de configuration (défaut : `~/.conso`) |
| `--sandbox` | Utiliser serveurs sandbox/homologation (défaut : production) |
| `-v`, `--verbose` | Afficher le détail des appels API |

---

## Commandes

### `lister` — Lister les sites inscrits

```bash
conso-registre lister
```

Affiche la table `UUID | PRM | PCE` de tous les sites du registre.

---

### `inscrire` — Inscrire un PRM ou un PCE

#### `inscrire prm PRM` — Inscrire un point électricité (Enedis SGE)

```bash
conso-registre inscrire prm <PRM> \
    --type CDC|IDX|ENERGIE|PMAX|ITC [-t ...] \
    (--nom NOM | --denomination RAISON_SOCIALE) \
    [--pce PCE [--force]] \
    [--site UUID [--force]]
```

| Option | Description |
|--------|-------------|
| `--type TYPE` (requis, répétable) | Types de flux : `CDC`, `IDX`, `ENERGIE`, `PMAX`, `ITC` |
| `--nom NOM` | Personne physique (exclusif avec `--denomination`) |
| `--denomination TEXT` | Personne morale |
| `--pce PCE` | Rattacher au site du PCE existant |
| `--site UUID` | Rattacher au site par UUID |
| `--force` | Ignorer la vérification de cohérence code postal |

#### `inscrire pce PCE` — Inscrire un point gaz (GRDF ADICT)

```bash
conso-registre inscrire pce <PCE> \
    --cp CODE_POSTAL \
    (--nom NOM | --denomination RAISON_SOCIALE) \
    [--email EMAIL] \
    [--avec-injections] \
    [--prm PRM [--force]] \
    [--site UUID [--force]]
```

| Option | Description |
|--------|-------------|
| `--cp CODE_POSTAL` (requis) | Code postal du site |
| `--nom NOM` | Personne physique |
| `--denomination TEXT` | Personne morale |
| `--email EMAIL` | Email de contact |
| `--avec-injections` | Inclure le périmètre injections gaz dans le droit d'accès |
| `--prm PRM` | Rattacher au site du PRM existant |
| `--site UUID` | Rattacher au site par UUID |
| `--force` | Ignorer la vérification de cohérence code postal |

---

### `ingerer` — Ingérer les données depuis les APIs

#### `ingerer elec` — Données Enedis (fichiers R6x/C68/NASS depuis le SFTP)

```bash
conso-registre ingerer elec \
    [--prm PRM [-p PRM ...]]
```

| Option | Description |
|--------|-------------|
| `--prm PRM` (répétable) | Filtrer sur ce(s) PRM uniquement (défaut : tous) |

#### `ingerer gaz` — Données GRDF ADICT

```bash
conso-registre ingerer gaz \
    [--pce PCE [-p PCE ...]]
```

| Option | Description |
|--------|-------------|
| `--pce PCE` (répétable) | Filtrer sur ce(s) PCE uniquement (défaut : tous) |

---

### `supprimer` — Supprimer un PRM, un PCE ou un site complet

#### `supprimer prm UUID` — Arrêt SGE + suppression données électricité

```bash
conso-registre supprimer prm 550e8400-e29b-41d4-a716-446655440000
```

#### `supprimer pce UUID` — Révocation ADICT + suppression données gaz

```bash
conso-registre supprimer pce 550e8400-e29b-41d4-a716-446655440000
```

#### `supprimer tout UUID` — Suppression complète du site

```bash
conso-registre supprimer tout 550e8400-e29b-41d4-a716-446655440000
```

Arrête le service SGE si un PRM est présent, supprime le fichier `.db` et l'entrée du registre.

---

## Exemples

```bash
# Lister les sites
cabal run conso-registre -- lister

# Inscrire un PRM avec courbe de charge et index (personne physique)
cabal run conso-registre -- inscrire prm 14xxxxxxxxxxxxxx \
    -t CDC -t IDX --nom "Jean Dupont"

# Inscrire un PCE rattaché au PRM existant (homologation)
cabal run conso-registre -- --sandbox inscrire pce 14yyyyyyyyyyyyyy \
    --cp 75001 --nom "Jean Dupont" --prm 14xxxxxxxxxxxxxx --avec-injections

# Ingérer toutes les données élec
cabal run conso-registre -- ingerer elec

# Ingérer gaz pour deux PCE spécifiques
cabal run conso-registre -- ingerer gaz --pce 14yyy --pce 14zzz

# Supprimer un site complet
cabal run conso-registre -- supprimer tout 550e8400-e29b-41d4-a716-446655440000
```
