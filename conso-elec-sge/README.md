# Consommation électrique via les webservices SGE d'Enedis

Client SOAP pour les webservices Enedis SGE (Système de Gestion des Échanges). Ce client fournit une interface en ligne de commande (CLI) avec affichage TUI (Text user interface). Ce client permet d'obtenir tous les types de données de consommation des points de consommation d'électricité gérés par Enedis.

## Prérequis

- GHC 9.6+, cabal
- Un contrat Enedis Tiers et donc les certificats TLS client Enedis (`.pem`) déposés dans `~/.conso/`
- Accès réseau aux URLs SGE Enedis (production et/ou homologation)

## Build

```bash
cabal build exe:conso-elec-sge    # CLI principale
cabal build exe:manage-rfiles      # Utilitaire de déchiffrement/chargement R-files
cabal test conso-elec-sge-test     # Suite de tests hspec
```

## Configuration

Fichier `~/.conso/conso-env.yaml` :

```yaml
sge:
  enedis:
    production:
      userB2b:    "mon_utilisateur"
      password:   "mon_mot_de_passe"
      contractId: "mon_contrat"
      key:        "cle.pem"          # relatif à ~/.conso/
      cert:       "cert.pem"
      url:        "https://..."
    homologation:
      userB2b:    "..."
      password:   "..."
      contractId: "..."
      key:        "cle-homo.pem"
      cert:       "cert-homo.pem"
      url:        "https://..."
    test:
      pointId:                          "14xxxxxxxxxxxxxx"
      nomClientFinalOuDenominationSociale: "..."
      numeroEtNomVoie:                  "..."
      codePostal:                       "xxxxx"
      codeInseeCommune:                 "xxxxx"
```

## Utilisation

```bash
cabal run conso-elec-sge -- <commande> [options]
```

Options globales disponibles sur toutes les commandes :

| Option | Description |
|--------|-------------|
| `--xml` | Affiche la réponse XML brute du webservice |
| `--raw` | Affiche la réponse Haskell non formatée (pPrint) |
| `CONSO_VERBOSE=1` | Active les logs de requête/réponse HTTP |

Sans `--xml` ni `--raw`, la réponse est affichée dans un TUI Brick scrollable (↑/↓, q/Esc pour quitter).

---

## Commandes

### Consultation d'un point

#### `info` — Données techniques et contractuelles (ConsulterDonneesTechniquesContractuellesV10)

```bash
conso-elec-sge info --point <PRM> [--autorisation]
```

| Option | Description |
|--------|-------------|
| `-p`, `--point PRM` | Identifiant PRM du point |
| `-a`, `--autorisation` | Inclure les données contractuelles (nécessite l'accord client) |

#### `recherche` — Recherche de points par critères (RechercherPointV20)

```bash
conso-elec-sge recherche [--voie TEXTE] [--code-postal XXXXX] [--commune XXXXX] [...]
```

| Option | Description |
|--------|-------------|
| `--voie TEXTE` | Numéro et nom de voie |
| `--escalier TEXTE` | Escalier / étage / appartement |
| `--batiment TEXTE` | Bâtiment |
| `--lieu-dit TEXTE` | Lieu-dit |
| `-c`, `--code-postal CPPPP` | Code postal |
| `-i`, `--commune XXXXX` | Code INSEE commune |
| `--siret SIRET` | Numéro SIRET |
| `--matricule TEXTE` | Matricule ou numéro de série |
| `--domaine BTINF\|BTSUP\|HTA\|HTB` | Domaine de tension |
| `--nom TEXTE` | Nom du client final |
| `--categorie PRO\|RES` | Catégorie client |
| `-r`, `--hors-perimetre` | Rechercher hors périmètre |

---

### Mesures (accès direct)

#### `mesures` — Mesures mensuelles (ConsulterMesuresV11)

```bash
conso-elec-sge mesures --point <PRM>
```

#### `mesuresdetail` — Mesures détaillées (ConsulterMesuresDetailleesV3)

```bash
conso-elec-sge mesuresdetail <courbe|pmax|energie|index> --point <PRM> \
    --debut YYYY-MM-DD --fin YYYY-MM-DD [options]
```

Options communes :

| Option | Description |
|--------|-------------|
| `-p`, `--point PRM` | Identifiant PRM |
| `--debut YYYY-MM-DD` | Date de début (incluse) |
| `--fin YYYY-MM-DD` | Date de fin (exclue) |
| `--sens INJECTION\|SOUTIRAGE` | Sens (défaut : SOUTIRAGE) |
| `--autorisation ACCORD_CLIENT\|SERVICE_ACCES\|EST_TITULAIRE` | Cadre d'accès (défaut : ACCORD_CLIENT) |
| `--corrigees` | Mesures corrigées |

Sous-commandes :

| Sous-commande | Grandeurs | Option supplémentaire |
|--------------|-----------|----------------------|
| `courbe` | `PA\|PRI\|PRC\|E\|TOUT` | |
| `pmax` | `PMA\|TOUT` | `--pas P1D\|P1M` (défaut : P1D) |
| `energie` | `EA\|ERC\|ERI` | |
| `index` | `EA\|ER\|ERC\|ERI\|DD\|DE\|DQ\|PMA\|TF\|TOUT` | |

---

### M023 — Demande de publication de fichiers

#### `m023 fines` — Flux R63–R66 (DemandePublicationMesuresFinesM23V10)

```bash
conso-elec-sge m023 fines -p <PRM> [-p <PRM2> ...] \
    --type COURBES|INDEX|ENERGIE|PMAX \
    --debut YYYY-MM-DD [--fin YYYY-MM-DD] \
    [--corrigees|--brutes] [--sens ...] [--cadre ...]
```

#### `m023 facturantes` — Flux R67 (DemandePublicationMesuresFacturantesM23V10)

```bash
conso-elec-sge m023 facturantes -p <PRM> [-p <PRM2> ...] \
    --debut YYYY-MM-DD [--fin YYYY-MM-DD] \
    [--sens ...] [--cadre ...]
```

#### `m023 itc` — Flux C68 (DemandePublicationInformationsTechniquesContractuellesM23V10)

```bash
conso-elec-sge m023 itc -p <PRM> [-p <PRM2> ...] [--sens ...] [--cadre ...]
```

---

### Services AccèsDonnées R6x (API v26+)

#### `lister` — Lister les services d'accès (RechercherServicesAccesDonneesV10)

```bash
conso-elec-sge lister --point <PRM>
```

#### `declarer` — Ouvrir un accès (CommanderServicesAccesDonneesV10)

```bash
conso-elec-sge declarer --point <PRM> --type CDC|IDX|ENERGIE|PMAX \
    [--sens ...] [--duree JOURS | --date-fin YYYY-MM-DD] \
    [--nom NOM | --denomination DENOM] \
    [--periodicite P1D|P7D|P1M]
```

`--periodicite` active le dépôt automatique sur SFTP. Sans cet argument : accès consultation uniquement.

#### `modifier` — Modifier les options (CommanderModificationOptionsServicesAccesDonneesV10)

```bash
conso-elec-sge modifier --point <PRM> --service <ID> \
    [--ajouter P1D|P7D|P1M] [--supprimer P1D|P7D|P1M] \
    [--corrigees|--non-corrigees]
```

#### `renouveler` — Renouveler un service (CommanderRenouvellementServicesAccesDonneesV10)

```bash
conso-elec-sge renouveler --point <PRM> --service <ID> [-s <ID2> ...] \
    [--duree JOURS | --date-fin YYYY-MM-DD] \
    [--nom NOM | --denomination DENOM]
```

#### `arreter` — Arrêter un service (CommanderArretServicesAccesDonneesV10)

```bash
conso-elec-sge arreter --point <PRM> --service <ID> [-s <ID2> ...]
```

---

### API antérieure à v26 (obsolète)

| Commande | Webservice |
|----------|-----------|
| `old-services` | RechercherServicesSouscritsMesuresV10 |
| `old-acces` | CommanderAccesDonneesMesuresV10 |
| `old-collecte` | CommanderCollectePublicationMesuresV30 |
| `old-arret` | CommanderArretServiceSouscritMesuresV10 |

---

## Exemples

```bash
# Infos techniques et contractuelles d'un point
cabal run conso-elec-sge -- info -p 14xxxxxxxxxxxxxx --autorisation

# Courbe de charge sur 7 jours (XML brut)
cabal run conso-elec-sge -- mesuresdetail courbe -p 14xxxxxxxxxxxxxx \
    -g PA --debut 2025-01-01 --fin 2025-01-08 --xml

# Demande M023 pour deux PRM
cabal run conso-elec-sge -- m023 fines -p 14xxx -p 14yyy \
    --type COURBES --debut 2025-01-01 --fin 2025-02-01

# Ouvrir un accès R6x avec publication quotidienne
cabal run conso-elec-sge -- declarer -p 14xxxxxxxxxxxxxx \
    --type CDC --duree 364 --periodicite P1D --nom "Jean Dupont"

# Mode verbose (logs HTTP)
CONSO_VERBOSE=1 cabal run conso-elec-sge -- lister -p 14xxxxxxxxxxxxxx
```
