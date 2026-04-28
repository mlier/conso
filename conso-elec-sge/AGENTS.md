# File objective

This file provides guidance to coding systems when working with code in this repository.

## Commands

```bash
# Build library + executable
cabal build exe:conso-elec-sge

# Build test suite only
cabal build conso-elec-sge-test

# Run the CLI
cabal run conso-elec-sge -- <command> [options]

# Run all tests
cabal test conso-elec-sge-test

# Run a specific test module (hspec pattern)
cabal test conso-elec-sge-test --test-option=--match --test-option="ConsulterDonneesTechniques"

# Get raw XML from a webservice (bypass TUI)
cabal run conso-elec-sge -- info --point <PRM> --autorisation --xml

# Verbose mode (logs request/response)
CONSO_VERBOSE=1 cabal run conso-elec-sge -- info --point <PRM> --autorisation
```

## Architecture

### Three-layer structure

```
src/   — Library: SOAP client (webservices + types)
app/   — Executable: CLI parser + TUI display
test/  — hspec-discover test suite
```

### SOAP infrastructure (`src/Conso/Fr/Elec/Sge/Sge.hs`)

Central module that all webservices depend on. Key types and functions:

- `SgeEnv` / `Sge` / `Test`: config read from `~/.conso/conso-env.yaml` (production, homologation, test PRM)
- `RequestType` / `ResponseType`: typeclasses that bind a request type to its URL, SOAPAction, XML serializer and response parser
- `wsRequest` / `wsRequestTest`: send request → `Either (String, String) b` (Left = SGE error code + label)
- `xmlRequest` / `xmlRequestTest`: send request → raw XML string
- `fixDoubleUtf8`: corrects double UTF-8 encoding present in some SGE server responses — applied automatically in `soapRequest`
- `checkXMLerror`: inspects the `<resultat code="SGTxxx">` element; `SGT200` = success, anything else = error tuple

### Webservice module pattern

Each webservice consists of two files:

1. **`MyWebserviceVN.hs`** — logic module:
   - `RequestType` instance: `configReq = ConfigRequest{ urlSge, soapAction, elementToXMLRequest }`
   - `ResponseType` instance: `configResp = ConfigResponse{ xmlTag, elementResponse }`
   - `initType` / `initTypeTest`: construct the request from credentials + parameters
   - Re-exports `wsRequest`, `xmlRequest`, `wsRequestTest`, `xmlRequestTest` from `Sge`

2. **`MyWebserviceVNType.hs`** — HaXml XSD-generated types:
   - Data types for request and response
   - `elementToXMLMyRequest`: XML serializer
   - `elementMyServiceResponse`: HaXml parser

### CLI flow (`app/Main.hs`)

`main` → `execParser` → pattern match `optCommand` → `docommand`:
1. Call `initType` (or module's `initType`) to build request from CLI args + credentials
2. `--xml` flag → `xmlRequest` → `putStrLn . prettyXml`
3. `--raw` flag → `wsRequest` → `pPrint`
4. Otherwise → `wsRequest` → `renderApp` (Brick TUI, scrollable with q/Esc/↑/↓)

Commands are organized as: `info`, `recherche`, `mesures`, `mesuresdetail`, `m023` (new API), `liste`, `declarer`, `modifier`, `renouveler`, `arret` (ACS v26+), plus `old-*` variants for deprecated APIs.

### TUI display (`app/Display/`)

- `Display.hs`: core helpers — `ustr` (Unicode-safe text widget), `sText` (like `simpleTypeText` but decodes XML entities), `field`/`maybeField`/`section`/`sectionActif`/`sectionTermine`/`renderError`, attribute names
- Each `*Display.hs` defines an orphan `Renderable` instance for one response type
- `Renderable.toWidget :: Either (String, String) a -> Widget ()` — Left renders an error box, Right renders the response

### Test structure (`test/`)

- `Main.hs`: single `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` line
- `SpecHelper.hs`: shared helpers — `testPointId`, `pendingOnNetworkError` (skip on network failure but preserve assertion failures), `shouldHaveCode`, `cleanupServices`
- `TestData.hs`: all PRM constants and date ranges centralized by webservice
- Each `*Spec.hs` tests production and homologation; homologation tests wrapped in `pendingOnNetworkError`
- SGT570 ("service déjà actif") is treated as acceptable for command tests via `isRightOrSgt570`

### Configuration

`~/.conso/conso-env.yaml` — parsed by `readEnv` → `ConsoEnvFile` wrapper (key path: `sge.enedis`):
```yaml
sge:
  enedis:
    production:   { userB2b, password, contractId, key, cert, url }
    homologation: { userB2b, password, contractId, key, cert, url }
    test:         { pointId, nomClientFinalOuDenominationSociale, numeroEtNomVoie, codePostal, codeInseeCommune }
```

TLS client certificates are at `~/.conso/<cert>` and `~/.conso/<key>` (paths relative to `~/.conso/`).
