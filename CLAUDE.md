# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.
Detailed topic docs live in `.cursor/docs/` — read the relevant one(s) for your current task.

## Critical Rules (Always Apply)

1. **NEVER edit files in `src-read-only/`** — these are generated from YAML specs via NammaDSL
2. **Always run `cabal build all`** after code generation to verify correctness
3. **Project uses `-Werror`** — all GHC warnings are compile errors (unused imports, dodgy imports, etc.)
4. **ID generation**: `newId <- generateGUID` (from `Kernel.Utils.Common`)
5. **Error handling**: `entity <- QEntity.findById id >>= fromMaybeM (EntityNotFound id.getId)`
6. **DB inserts**: Call `create` from `Storage.Queries` directly; never wrap single creates in `runInTransaction`
7. **YAML imports**: Use full module paths (e.g., `Domain.Types.IntegratedBPPConfig`), not short names
8. **Beckn tags**: Must be defined in `Backend/lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs` before use
9. **Orphan instances**: Go in `Domain/Types/Extra/*.hs` files
10. **Logging**: Use `logInfo`, `logDebug`, `logError` from `Kernel.Utils.Logging`
11. **YAML Storage constraints — `!SecondaryKey` must be quoted**: Write `fieldName: "!SecondaryKey"`, never unquoted `fieldName: !SecondaryKey`. Unquoted `!Tag` values are parsed as YAML tags and silently dropped by the NammaDSL parser, producing an empty `enableKVPG` secondary-key list in the generated Beam file → intermittent-empty KV reads at runtime. Use `!SecondaryKey` (forced) when the query is in a hand-written Extra file; plain `SecondaryKey` only when the field appears in a YAML-declared query. The `yaml-constraint-tags` pre-commit hook enforces the quoting. Deny list: `merchantId`, `merchantOperatingCityId`, `status` cannot be secondary keys regardless of quoting.

## Build & Development

```bash
# Environment setup (one-time, from project root)
ln -sf .envrc.backend .envrc && direnv allow

# Backend
cd Backend
cabal build all                  # Build everything
cabal build <package-name>       # Build specific package (e.g., rider-app)

# Code generation (run from Backend/ inside nix shell)
, run-generator                  # Only changed specs
, run-generator --all            # All specs
, run-generator --apply-hint     # With HLint auto-fixes

# Utilities
, run-mobility-stack-dev         # Start external services (Postgres, Redis, Kafka, etc.)
, ghcid lib/<package-name>       # Fast compile feedback loop
, hpack                          # Regenerate .cabal files from package.yaml
, kill-svc-ports                 # Kill lingering service processes
```

Full build details: `.cursor/docs/02-build-and-dev.md`

## Architecture — Quick Reference

| Service | Port | Path |
|---------|------|------|
| rider-app (BAP) | 8013 | `app/rider-platform/rider-app/` |
| dynamic-offer-driver-app (BPP) | 8016 | `app/provider-platform/dynamic-offer-driver-app/` |
| driver-offer-allocator | 9996 | `app/provider-platform/dynamic-offer-driver-app/Allocator/` |

| Database | Rider Schema | Driver Schema |
|----------|-------------|---------------|
| PostgreSQL | `atlas_app` | `atlas_driver_offer_bpp` |
| Redis | Single 6379 / Cluster 30001 | Same |

BAP (rider-app) initiates BECKN calls → BPP (driver-app) responds with callbacks.
ACL modules translate between BECKN protocol types and internal domain types.

Full architecture: `.cursor/docs/01-architecture-overview.md`

## Key Directory Patterns

| Purpose | Path Pattern |
|---------|-------------|
| Domain types | `*/src-read-only/Domain/Types/` |
| Business logic | `*/src/Domain/Action/UI/` |
| DB queries | `*/src-read-only/Storage/Queries/` |
| Extra queries | `*/src/Storage/Queries/` |
| Cached queries | `*/src/Storage/CachedQueries/` or `*/src-read-only/Storage/CachedQueries/` |
| YAML API specs | `*/spec/API/*.yaml` |
| YAML Storage specs | `*/spec/Storage/*.yaml` |
| Beckn ACL | `*/src/Beckn/ACL/` |
| SharedLogic | `*/src/SharedLogic/` |
| Migrations | `dev/migrations/<service-name>/` |

## Code Generation (NammaDSL)

- **API specs**: `spec/API/*.yaml` → generates `src-read-only/API/`
- **Storage specs**: `spec/Storage/*.yaml` → generates `src-read-only/Domain/Types/`, `Storage/Beam/`, `Storage/Queries/`
- Generator also creates stub files in `src/Domain/Action/UI/` for business logic
- Use camelCase for endpoint paths, full module paths for imports
- Common auto-imported types: `Text`, `Maybe`, `Int`, `Bool`, `Id`, `UTCTime`, `HighPrecMoney`, `Currency`

Full DSL reference: `.cursor/docs/07-namma-dsl.md`

## Haskell Conventions

- `cabal build <target>` for checks; `cabal repl` alone doesn't guarantee compilability
- If a `.hs` file is deleted, run `, hpack` to update `.cabal` file
- Use `fromTType`/`toTType` in YAML for domain-to-beam type transformations
- `extraOperations`: `EXTRA_QUERY_FILE`, `EXTRA_DOMAIN_TYPE_FILE`, `EXTRA_CACHED_QUERY_FILE`

Full conventions: `.cursor/docs/15-conventions.md`

## BECKN Protocol

Flow: `search` → `on_search` → `select` → `on_select` → `init` → `on_init` → `confirm` → `on_confirm`
Protocol details: `.cursor/docs/05-beckn-protocol-flow.md`
Ride lifecycle: `.cursor/docs/06-ride-flow.md`

## FRFS (Public Transport)

- BAP-only — BPPs are external operators (CMRL, CRIS, EBIX)
- Two paths: ONDC (async Beckn) and Direct (synchronous API)
- `mkCloudBapUri` in `Beckn/ACL/FRFS/Utils.hs` handles multi-cloud callback routing
- Constraint propagation: add to type aliases in `ExternalBPP/CallAPI/Types.hs` and `SharedLogic/CallFRFSBPP.hs`

Full FRFS details: `.cursor/docs/10-frfs-public-transport.md`

## Commit / Branch Conventions

```
Commit: <sub-project>/<type>: <issue-number> <summary>
Branch: <sub-project>/<type>/<issue-number><description>
Types: feat, fix, chore, ci, docs, perf, refactor, test
```

## Deep Dive Docs (`.cursor/docs/`)

| Doc | Read when working on... |
|-----|------------------------|
| `01-architecture-overview.md` | Service map, ports, packages |
| `02-build-and-dev.md` | Build commands, nix, compilation |
| `03-rider-app.md` | Customer-facing features |
| `04-driver-app.md` | Driver-facing features |
| `05-beckn-protocol-flow.md` | BECKN protocol interactions |
| `06-ride-flow.md` | End-to-end ride lifecycle |
| `07-namma-dsl.md` | Creating/modifying YAML specs |
| `08-database-patterns.md` | Queries, caching, migrations |
| `09-dashboards.md` | Dashboard services |
| `10-frfs-public-transport.md` | Metro/bus/public transport |
| `11-libraries.md` | Shared libraries |
| `12-multi-cloud.md` | Multi-cloud, KV connector, Redis |
| `13-external-integrations.md` | Juspay, OSRM, Idfy, SMS, FCM |
| `14-testing-and-debugging.md` | Debugging patterns |
| `15-conventions.md` | Haskell conventions |
| `16-status-definitions.md` | Status enums, state transitions |
| `17-testing-framework.md` | Config sync, integration tests, mock servers, test tools |

---

# This fork: Movin (Mauritania and Algeria on one stack, since 2026-09-13)

Everything above is upstream Namma Yatri and still applies to the Haskell
services. This section is what is different here, and it is mostly about what
**not** to look for.

## What we actually run

The whole deployment is `Backend/dev/local-stack` — Docker Compose, one VPS,
~20 containers. Read that directory's README first; it is the real
documentation for this fork. `./setup.sh` brings it up.

Three services were replaced so the stack needs no Google account and no bill:

| Upstream | Here |
|---|---|
| Google Directions | **OSRM**, our own, on the country OSM extract |
| Google Maps tiles | **tileserver-gl**, same extract |
| Google Places / geocoding | **`maps-shim`**, answering from a Postgres place index |

The trick that makes those possible is worth knowing before adding a fourth:
**service endpoints are database config, not compiled in.** `Maps_Google` in
`atlas_app.merchant_service_config` carries `"googleMapsUrl"`, and pointing it
at `maps-shim` is the entire integration.

`Sms_MyValueFirst` is the same shape and was the obvious place to put a real SMS
gateway — but **that is not where it went**, and the reason generalises. The
backend's `useFakeSms = Some 7891` short-circuits the whole SMS path, so
repointing the config changes nothing until that setting goes, and it is in
dhall, in the image. Instead `auth-guard` in front makes the code, sends it
through Moorsyl, checks it, and substitutes 7891 before forwarding. The backend
still believes in its fixed code and was never touched. **When a config knob sits
behind a compiled-in switch, the config knob is not the integration point.**

## The rider app is NOT in this repository

`Frontend/` here is upstream's PureScript app and **we do not build or ship
it.** The Movin DZ rider app is React Native (Expo), in its own private repo,
cloned alongside this one:

    ~/ny-algeria-passenger      github.com/nammayatri-algeria/namma-yatri-frontend

Anything about screens, the APK, the signing key or the map UI belongs there.

## Binaries, and why patches go around the backend

The services run as **prebuilt images** from a CI job
(`.github/workflows/algeria-backend-build.yml`, free GitHub Actions).

**Rebuilding is affordable, and the reason to avoid it is not the clock.**
Measured: **44 minutes cold**, 33 on the company org, 8 warm. The
`timeout-minutes: 350` in that workflow is the *cap*, and reading it as a
duration is how this file previously said "~6-hour budget" — which made every
backend change sound like a day's work and led to at least one wrong answer to
the client.

The real cost is that a rebuild produces **new binaries, and every measurement
in this project was taken against the current ones**. So: batch backend changes
into one run, and re-prove the ride flow afterwards. Prefer config, SQL or a
shim when one will do — most things have turned out not to need Haskell at all
(routing, maps, geocoding, push, the fare policy, the driver's answer window).

So: prefer config, SQL, or a shim in front. The OTP attempt limit lives in
nginx for exactly this reason, not in the Haskell that already had the counter.

**The running binary is older than this tree, and on some paths they disagree
outright.** `Backend/dev/local-stack/bin/MANIFEST.txt` names the build ref
(`03a7531`), which is an *ancestor* of this branch. Upstream has since replaced
whole subsystems. Measured case: the tree says driver positions come from the
location-tracking service, a separate Rust binary we do not run; the deployed
binary still has `POST /ui/driver/location` writing Postgres directly, and that
is what actually serves us.

So when the question is "what does the server do", **ask the server**, not the
source. It publishes its own route list at `/openapi`, and `strings` on the
binaries in `bin/` settles anything else. Reading the tree instead has already
produced one confident, wrong conclusion — that a driver app required deploying
another service first. See the driver API section of the local-stack README.

## Country-specific data

**Two countries at once since 2026-09-13.** The pilot moved Algeria →
Mauritania on 2026-09-03 by *replacing* one with the other; the client then
chose to run both. Mauritania is live; Algeria is built, priced and routed, and
closed to sign-in (`OPEN_COUNTRIES` in the auth guard) until it has an SMS
provider. The whole design — one rider merchant, one driver merchant per
country, and the search-lock race that design exposed — is in the local-stack
README, section *Two countries*. Read it before touching merchants, tariffs,
the registry or the map.

- **One driver merchant per country:** `favorit0-…` is Mauritania,
  `algeria0-0000-0000-0000-00000algeria` is Algeria. Both tariff files and every
  per-merchant script are keyed by `merchant_id` — an unkeyed statement reprices
  the other country.
- The map is one combined build, `MAP_COUNTRY=algeria-mauritania`, now written
  in `.env` (it used to be typed inline, and a plain `docker compose up` would
  have reverted it). `COUNTRY=` on `osrm-prepare.sh` / `tiles-prepare.sh` still
  builds one country; `maps-two-countries.sh` builds both. **Never
  `geocoder-prepare.sh load`**: it drops the place index and its reviewed Arabic
  names; add a country with `geocoder/append-country.sql`.
- **There are TWO service areas, not one.** `atlas_app.geometry` +
  `atlas_app.merchant.origin_restriction` for the rider, and
  `atlas_driver_offer_bpp.geometry` + its own two merchants for the provider.
  Switching only the rider's leaves searches reaching the BPP and being dropped
  there, with no error and no estimate. `mauritania-geofences.sql`.
- `serviceable: true` means "inside the country", not "a car will come" — the
  Majabat al-Koubra is serviceable and several hundred km from any driver.
- Phone numbers, and they are each other's inverse: **Mauritania** `+222`,
  eight digits, no trunk prefix, mobiles start 2/3/4 and never `x5`;
  **Algeria** `+213`, nine digits typed and sent as ten WITH the trunk zero,
  mobiles start 5/6/7. The backend accepts either (`Or` patches); the app's
  `src/lib/country.ts` pairs code and length; the guard decides which country
  is open.
- Tariffs: `mauritania-tariff.sql` (MRU, the Algerian table × 0.30, a
  **placeholder**) and `algeria-tariff.sql` (DA, the Mauritanian ÷ 0.30).
  Each is keyed to its own merchant.
- Test fleets: `./seed-mauritanian-fleet.sh` (two per sellable variant in
  Nouakchott, driven by the simulator — never sign in as one) and the pilot's
  twelve `+213` drivers parked in Algiers under `algeria0`. Algerian test
  accounts that sign in without SMS: `./algerian-test-accounts.sh` — they must
  be removed before Algeria opens.
- **With more than one merchant in one process, audit every per-message
  lock.** The search handler's `whenWithLockRedis` on the message id silently
  dropped whichever merchant arrived second; patched to merchant + message.

## Backups

`./backup.sh` — nightly, encrypted, off to cloud storage. It deliberately skips
the 155 MB place index (rebuildable) and deliberately includes the **passetto**
database, without which restored phone numbers are unreadable ciphertext. See
the header of that script.

## Traps that have each cost an afternoon

- **Driver locations go stale silently.** The dispatch pool ignores old
  positions, so search returns zero estimates with no error anywhere. Run
  `./setup.sh drivers` before any demo.
- **`docker exec -i` inside `ssh host "bash -s" <<EOF` eats the rest of the
  script** from stdin. Drop the `-i`.
- **`ufw limit` rejects the sixth SSH connection in 30 seconds** — exit 255 and
  no output. Batch remote work into one connection.
- **Replacing a bind-mounted file breaks the mount.** `tar -x` unlinks the
  inode; the container keeps serving the old file while `nginx -t` passes.
  Use `scp`, which truncates in place.
- **Editing a script through the Windows UNC path strips its exec bit.**
- **A heredoc terminator does not survive the Windows→WSL hop.** `<<'PYEOF'` …
  `PYEOF` fails with *"unexpected EOF while looking for matching `'`"* — the
  terminator line arrives with a carriage return and never matches, so the shell
  swallows the whole script. Write the file, then run the file. Same for
  `python3 - <<EOF`: it may run and still no-op, because `str.replace` that
  matches nothing is silent. **Assert `count == 1` on every scripted edit.**
- **Backslash escapes and pipes are rewritten on the same hop**, and both
  failures lie about their cause. `sed -i "s/\r$//"` arrives as `s/r$//` and
  **deletes a trailing letter `r` from every line** — it turned `FROM …ride r`
  into `FROM …ride` and the error read *missing FROM-clause entry for table
  "r"*, which looks like a bad query. That sed was never needed anyway: files
  written from the editor are already LF, and `file x.sh` says so. `grep -E
  "a|b"` splits at the pipe and tries to run `b` as a command. Same fix as
  above: put it in a file and run the file. `<` is worse — it is reserved in
  PowerShell and never reaches bash at all, so `ssh ny "bash -s" < script.sh`
  silently does nothing. Wrap that in a script too.
- **A lone connection timeout from the laptop is not a result** — but two in a
  row, while `ssh` to the same box still works, means the guard in front is
  rate-limiting you. Run the probe *on* the VPS instead of against it.
- **BECKN could not parse a negative coordinate**, and had not been able to
  since 2023. `Beckn/Types/Core/Taxi/Common/Gps.hs` read the gps string with
  Parsec's `P.float`, which is unsigned, so every longitude west of Greenwich
  failed. Algeria is at +3 and this was invisible for the whole pilot;
  Nouakchott is at −15.9 and no search reached the driver pool. **The provider
  logged nothing** — it answered the gateway 400, which from its side is
  correct behaviour. The reason existed only in the response body the *gateway*
  received. Patched now, but the shape of the lesson generalises: when a
  component reports no error, read what its caller got back.
- **Whatever you are grepping the logs for, parse them as JSON instead.** The
  container log is one JSON object per line and the useful message is inside a
  `"log"` field with escaped quotes. Three separate greps truncated the answer
  above at the first `\"` before it was found.
- **`npx prettier` is not part of the app project.** `package.json` has only
  `expo lint` and there is no prettier config, so prettier runs with its own
  defaults — double quotes, 80 columns — against a codebase written with single
  quotes at 100. Running `--write` on two files rewrote 839 lines for a
  two-line change. A tool absent from `package.json` is not this project's
  standard.
