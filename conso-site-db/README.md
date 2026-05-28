# conso-site-db

Bibliothèque Haskell pure : registre SQLite central faisant correspondre chaque site (PRM Enedis et/ou PCE GRDF) à un UUID stable.

Aucun exécutable — consommée par `conso-site-db-elec`, `conso-site-db-gaz` et `conso-registre`.

## Build

```bash
cabal build conso-site-db
```

## Architecture

**Modèle deux-tiers :**

- `~/.conso/registry.db` — base unique : table `site_registry` (UUID ↔ PRM/PCE/Label)
- `~/.conso/sites/{XX}/{YY}/{uuid}.db` — base par site, shardée sur 2 niveaux hexadécimaux

## Modules exposés

| Module | Rôle |
|--------|------|
| `Conso.Fr.SiteDB.Types` | Newtypes : `SiteId`, `Prm`, `Pce`, `SiteLabel`, `SiteRef` |
| `Conso.Fr.SiteDB.Registry` | Façade : `openRegistry`, `withRegistry` |
| `Conso.Fr.SiteDB.Registry.Operations` | CRUD complet sur `site_registry` |
| `Conso.Fr.SiteDB.Storage.Connection` | Connexion bas niveau par site (`openSiteDb`, `siteDbPath`) |
| `Conso.Fr.SiteDB.Orchestration.Types` | `GetCodePostal`, `InscriptionPrmParams`, `InscriptionPceParams`, `TypeFlux` |
| `Conso.Fr.SiteDB.Orchestration.Adresses` | `verifierCoherence` |
| `Conso.Fr.SiteDB.Orchestration.Desinscription` | `desinscrirePrm`, `desinscrirePce`, `supprimerSite` |

## Types principaux

```haskell
newtype SiteId = SiteId UUID
newtype Prm    = Prm Text   -- 14 chiffres Enedis
newtype Pce    = Pce Text   -- 14 chiffres GRDF

data SiteRef = SiteRef
  { srSiteId            :: SiteId
  , srPrm               :: Maybe Prm
  , srPce               :: Maybe Pce
  , srLabel             :: Maybe SiteLabel
  , srGazAvecInjections :: Bool
  }

-- Abstraction fournie par les extensions pour obtenir un code postal
type GetCodePostal = Text -> IO (Either String Text)
```

## API CRUD

Toutes les opérations prennent une `Connection` SQLite ouverte via `withRegistry` :

```haskell
-- Création et recherche
createSite              :: Connection -> Maybe Prm -> Maybe Pce -> Maybe SiteLabel -> IO SiteId
lookupByPrm             :: Connection -> Prm -> IO (Maybe SiteId)
lookupByPce             :: Connection -> Pce -> IO (Maybe SiteId)
lookupBySiteId          :: Connection -> SiteId -> IO (Maybe SiteRef)
lookupOrCreateByPrm     :: Connection -> Prm -> IO SiteId
lookupOrCreateByPce     :: Connection -> Pce -> IO SiteId

-- Rattachements
linkPrm   :: Connection -> SiteId -> Prm -> IO ()
linkPce   :: Connection -> SiteId -> Pce -> IO ()
unlinkPrm :: Connection -> SiteId -> IO ()
unlinkPce :: Connection -> SiteId -> IO ()

-- Administration
listSites              :: Connection -> IO [SiteRef]
deleteFromRegistry     :: Connection -> SiteId -> IO ()
setGazAvecInjections   :: Connection -> SiteId -> Bool -> IO ()
```

## Schéma SQLite (registry.db)

```sql
CREATE TABLE site_registry (
  uuid                TEXT PRIMARY KEY,
  prm                 TEXT UNIQUE,
  pce                 TEXT UNIQUE,
  label               TEXT,
  created_at          TEXT NOT NULL,
  gaz_avec_injections INTEGER NOT NULL DEFAULT 0
);
```

## Orchestration

`verifierCoherence` compare les codes postaux d'un PRM et d'un PCE via les callbacks. `GetCodePostal` fournis par les extensions, et retourne une `VerifAdresse` (cohérent/incohérent/inconnu).

Les fonctions de désinscription (`desinscrirePrm`, `desinscrirePce`, `supprimerSite`) acceptent des `DesinscriptionCallbacks` injectés par `conso-registre` pour découpler la logique d'arrêt des services (SGE, ADICT) de la suppression des données.
