# Consommation gaz via les webservices ADICT de GRDF

Client REST pour les webservices GRDF ADICT (Accès aux Données Individuelles de Consommation et de Transfert). Ce client fournit une une interface en ligne de commande (CLI) avec affichage TUI (Text user interface). Ce client permet d'obtenir les données de consommation gaz et gérer les droits d'accès tiers.

## Prérequis

- GHC 9.6+, cabal
- Credentials OAuth2 GRDF (Client Credentials) : `clientId` + `clientSecret`
- Accès réseau aux URLs ADICT GRDF (bac à sable et/ou production)

## Build

```bash
cabal build exe:conso-gaz-adict    # CLI principale
cabal test conso-gaz-adict-test    # Suite de tests hspec
```

## Configuration

Fichier `~/.conso/conso-env.yaml` :

```yaml
adict:
  grdf:
    sandbox:
      clientId:     "votre_client_id_sandbox"
      clientSecret: "votre_secret_sandbox"
      tokenUrl:     "https://sso-sandbox.grdf.fr/oauth/authorize"
      adictUrl:     "https://api-sandbox.grdf.fr/adict/v2"
    production:
      clientId:     "votre_client_id_prod"
      clientSecret: "votre_secret_prod"
      tokenUrl:     "https://sso-prod.grdf.fr/oauth/authorize"
      adictUrl:     "https://api.grdf.fr/adict/v2"
    testData:
      testPce:      "14345678901234"  # PCE du jeu de données sandbox
```

L'authentification utilise le flow OAuth2 **Client Credentials**. Le token est automatiquement
rafraîchi à expiration ; aucune action manuelle requise.

## Utilisation

```bash
cabal run conso-gaz-adict -- <commande> [options]
```

Par défaut les commandes s'exécutent sur le **bac à sable** GRDF. Ajouter `--prod` pour
utiliser le serveur de production.

Options globales disponibles sur toutes les commandes :

| Option | Description |
|--------|-------------|
| `--prod` | Serveur de production (défaut : bac à sable) |
| `--raw` | Affiche la réponse Haskell non formatée (pPrint) |
| `--debug` | Log des requêtes HTTP sur stderr |
| `--verbose` | Log des corps de réponse JSON sur stderr |

Sans `--raw`, la réponse est affichée dans un TUI Brick scrollable (↑/↓, q/Esc pour quitter).

---

## Commandes

### Données de consommation

#### `conso` — Consommations publiées

```bash
conso-gaz-adict conso --pce PCE [--periode PERIODE | --debut YYYY-MM-DD --fin YYYY-MM-DD]
```

#### `conso-info` — Consommations informatives

```bash
conso-gaz-adict conso-info --pce PCE [--periode PERIODE | --debut YYYY-MM-DD --fin YYYY-MM-DD]
```

#### `injection` — Injections publiées

```bash
conso-gaz-adict injection --pce PCE [--periode PERIODE | --debut YYYY-MM-DD --fin YYYY-MM-DD]
```

**Formats de période** (`--periode`) :

| Exemple | Signification |
|---------|--------------|
| `2024` | Année entière |
| `2024-06` | Mois de juin 2024 |
| `2024-W23` | Semaine ISO 23 de 2024 |

Sans `--periode`, utiliser `--debut` et `--fin` pour une plage libre.

---

### Données contractuelles et techniques

#### `contrat` — Données contractuelles

```bash
conso-gaz-adict contrat --pce PCE
```

#### `tech` — Données techniques

```bash
conso-gaz-adict tech --pce PCE
```

---

### Droits d'accès tiers

#### `liste` — Consulter les droits d'accès

```bash
conso-gaz-adict liste [--role ROLE] [--pce PCE] [--statut STATUT] [--etat ETAT]
```

Sans filtre : retourne tous les droits (GET). Avec au moins un filtre : recherche filtrée (POST).

| Option | Valeurs possibles |
|--------|------------------|
| `--role` | `acf`, `dcf`, `aci`, `dci` (voir tableau des rôles ci-dessous) |
| `--pce` | Identifiant PCE |
| `--statut` | Statut de contrôle de la preuve |
| `--etat` | `Active`, `AValider`, `Revoquee`, `AReverifier`, `Obsolete`, `Refusee` |

#### `declarer` — Déclarer un droit d'accès

```bash
conso-gaz-adict declarer --pce PCE --role ROLE --cp CODE_POSTAL \
    [--nom NOM | --raison RAISON_SOCIALE] \
    [--email EMAIL] [--tel TEL] \
    [--1an | --2ans | --3ans] \
    [--debut-acces YYYY-MM-DD] [--fin-acces YYYY-MM-DD] \
    [--debut-conso YYYY-MM-DD] [--fin-conso YYYY-MM-DD] \
    [--contractuelles] [--techniques] [--informatives] [--publiees]
```

| Option | Description |
|--------|-------------|
| `--pce PCE` | Identifiant du PCE |
| `--role ROLE` | Rôle du tiers (voir tableau ci-dessous) |
| `--cp CODE_POSTAL` | Code postal du titulaire |
| `--nom NOM` | Nom du client final (particulier) |
| `--raison RAISON` | Raison sociale (professionnel) |
| `--email EMAIL` | Email de contact |
| `--tel TEL` | Téléphone de contact |
| `--1an` / `--2ans` / `--3ans` | Durée automatique (calcule toutes les dates) |
| `--debut-acces` / `--fin-acces` | Dates d'accès explicites |
| `--debut-conso` / `--fin-conso` | Dates de périmètre de consommation |
| `--contractuelles` | Inclure les données contractuelles dans le périmètre |
| `--techniques` | Inclure les données techniques |
| `--informatives` | Inclure les consommations informatives |
| `--publiees` | Inclure les consommations publiées |

Avec `--1an` / `--2ans` / `--3ans` : début accès = aujourd'hui, fin accès = aujourd'hui + N ans,
début conso = aujourd'hui − 5 ans, fin conso = aujourd'hui + N ans.

#### `revoquer` — Révoquer un droit d'accès

```bash
conso-gaz-adict revoquer --id UUID
```

#### `preuves-attente` — Droits en attente de preuve

```bash
conso-gaz-adict preuves-attente
```

#### `preuve` — Transmettre une preuve de consentement

```bash
conso-gaz-adict preuve --id UUID --fichier CHEMIN
```

Formats acceptés : PDF ou image. Taille maximale : 4 Mo.

---

### Rôles tiers

| Alias court | Valeur complète |
|-------------|----------------|
| `acf` | `AUTORISE_CONTRAT_FOURNITURE` |
| `dcf` | `DETENTEUR_CONTRAT_FOURNITURE` |
| `aci` | `AUTORISE_CONTRAT_INJECTION` |
| `dci` | `DETENTEUR_CONTRAT_INJECTION` |

---

## Exemples

```bash
# Consommations publiées sur une année (bac à sable)
cabal run conso-gaz-adict -- conso --pce 14345678901234 --periode 2024

# Consommations sur une plage libre (production)
cabal run conso-gaz-adict -- conso --pce 14345678901234 \
    --debut 2024-01-01 --fin 2024-06-30 --prod

# Données techniques d'un PCE
cabal run conso-gaz-adict -- tech --pce 14345678901234

# Lister tous les droits d'accès actifs
cabal run conso-gaz-adict -- liste --etat Active

# Déclarer un droit pour 2 ans avec tous les périmètres
cabal run conso-gaz-adict -- declarer --pce 14345678901234 \
    --role acf --cp 75001 --nom "Jean Dupont" \
    --2ans --contractuelles --techniques --publiees --informatives

# Révoquer un droit
cabal run conso-gaz-adict -- revoquer --id 550e8400-e29b-41d4-a716-446655440000

# Transmettre une preuve de consentement
cabal run conso-gaz-adict -- preuve --id 550e8400-e29b-41d4-a716-446655440000 \
    --fichier /tmp/consentement.pdf

# Mode debug (log des requêtes HTTP)
cabal run conso-gaz-adict -- liste --debug
```
