# Dashboard APIs served directly by the application servers

## Summary

Dashboard traffic previously reached `driver-app` / `rider-app` through
`provider-dashboard`. That service authenticated the operator, authorised the
endpoint, wrote an audit row, then forwarded the request and held a connection
open for the whole downstream call — a blocking hop that performed no work of
its own.

The application servers now perform that verification themselves. Every
dashboard route is served directly — **934 routes: 567 provider-side, 367
rider-side**, enumerated from the servant types the servers compile from.

```
before   browser -> provider-dashboard -> driver-app / rider-app -> app DB
after    browser -> driver-app / rider-app -> app DB
                          |
                          +-> dashboard DB  (verify session, check capability, audit)
```

- No new service is introduced. The application servers read `atlas_dashboard`,
  the database `provider-dashboard` already used.
- The proxied tree in `provider-dashboard` is unchanged and still serves the
  same paths. Both are live at once, so cutover is a routing change and
  rollback needs no deploy.

---

## Repositories touched

This change spans three repositories. They must be released in order:
**namma-dsl → shared-kernel → nammayatri.**

### 1. namma-dsl — code generation

| Addition | Purpose |
|---|---|
| `SERVANT_API_DASHBOARD_AUTH`, `API_TREE_DASHBOARD_AUTH` | Two generator types emitting the app-side `API/Action/DashboardAuth/…` tree |
| `_appServerDashboardAuth :: Maybe Bool` | Per-spec-folder opt-in. `Nothing`/`False` preserves existing behaviour |
| `_servantApiDashboardAuth :: FilePath` | Output path for the generated tree |
| `_capabilityBaseline :: Maybe String` | Baseline file for capability-id drift detection |
| `hasExecutableSql` | Suppresses generated migration files containing only comments |
| `ApiTokenInfo` → `ApiTokenInfo Domain.Types.AccessMatrix.UserActionType` | `ApiTokenInfo` is parameterised by the action union of the package owning the endpoint (`Generator/Haskell/Common.hs`) |
| `castEndpoint` → `Domain.Types.Transaction.ActionAPI` | The 10 per-platform `Endpoint` constructors collapsed into a single `ActionAPI uat` (`Generator/Haskell/Dashboard/DomainHandler.hs`) |

Every `dsl-config.dhall` in nammayatri declares the new fields, which is why
otherwise-unrelated libraries appear in the diff.

### 2. shared-kernel — dashboard database access

| Module | Addition |
|---|---|
| `Kernel.Beam.Types` | `PsqlDashboardDbCfg`, `PsqlDashboardReplicaDbCfg`, `DashboardDbEnabled` option entities |
| `Kernel.Beam.Functions` | `runInDashboardDb`; `getDashboardDbConfig` / `getDashboardReplicaDbConfig`; the master, replica and read-config resolvers are now dashboard-aware |
| `Kernel.Beam.Connection.EnvVars` | `postgresDashboardConnectionName`, `postgresDashboardReplicaConnectionName` |
| `Kernel.Beam.Connection.Postgres` | `prepareDashboardDbConnections` (replica optional) |
| `Kernel.Beam.Connection.Flow` | `prepareDashboardDbForApp` |

`runInDashboardDb` scopes a block of queries to `atlas_dashboard` and restores
the previous scope with `finally`, so an exception cannot leak the dashboard
connection into surrounding application queries. Registration is opt-in — an
app whose config supplies no dashboard block resolves exactly as before.

### 3. nammayatri — the migration itself

- **The Cabal cycle is removed by splitting the action union, not by widening
  columns.** `lib-dashboard` previously depended on both application packages, so
  an application server linking it was a Cabal cycle — a hard blocker. The
  coupling came from two modules used as Beam *column types*: `UserActionType`
  and `Transaction.Endpoint`.

  Each package now owns the enum for the endpoints it serves:

  | Package | Type | Constructors |
  |---|---|---|
  | `driver-app` | `Domain.Types.AccessMatrix.UserActionType` | 6 × `PROVIDER_*` |
  | `rider-app` | `Domain.Types.AccessMatrix.UserActionType` | 4 × `RIDER_*` + 12 × `BHARAT_TAXI_*` |
  | `lib-dashboard` | `Domain.Types.DashboardActionType` | 4 × `SPECIAL_ZONE_*` |

  ```
            shared-services
                   ↓
            lib-dashboard          ← names NO app type
              ↙        ↘
       driver-app    rider-app     ← each owns its own action union
  ```

  Neither application package depends on the other, and `lib-dashboard` depends
  on neither. **The columns remain enums** — they are not stored as `Text`. The
  only text conversion is `showUserActionType`, in the `IsUserActionType` class.

  Endpoint id strings are **byte-identical to before**. This is load-bearing:
  `capability_endpoint` rows key off them and were not migrated.

- **`lib-dashboard-api` and `dashboard-helper-api` no longer exist.** An earlier
  revision of this work quarantined the offending modules in a new
  `lib-dashboard-api` package sitting *above* the application servers. That is
  superseded: with the union split there is nothing to quarantine, so both that
  package and `CommonAPIs` (`dashboard-helper-api`) were deleted and their
  contents moved into `lib-dashboard`, driver-app and rider-app.

- `lib-dashboard`'s auth is polymorphic in the action union
  (`Tools/Auth/ApiAuth.hs`), so route definitions keep their existing 3-argument
  shape: each owning package defines
  `type ApiAuth sn ae uat = Auth.ApiAuthFor UserActionType sn ae uat`.

- Generated `API/Action/DashboardAuth/…` trees on both application servers.
- The login / user-administration tree, and the ported dashboard-side logic.

---

## How a request is served

Both application servers mount `/direct-dashboard/{merchantId}/{city}/…`
(`API.DirectDashboard`), mirroring the dashboard's public paths exactly. Each
route is guarded by the `DashboardUserAuth` combinator, which carries the
endpoint identity as a type-level string — the value stored in
`capability_endpoint`:

```haskell
DashboardUserAuth 'DRIVER_OFFER_BPP_MANAGEMENT
  "PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LOCATION"
```

Per request the combinator:

1. resolves the operator's session,
2. enforces the capability registered for that endpoint (**fails closed**),
3. checks merchant and city scope,
4. writes the audit row,
5. hands the handler a resolved `DashboardUser`.

All of it runs against the dashboard database via `runInDashboardDb`.

The login tree is served by **driver-app only** — it mounts `lib-dashboard`'s
`API.DashboardLogin`, which aggregates all eleven native modules (`Person`,
`Registration`, `EmailVerification`, `Roles`, `Merchant`, `Capability`,
`ResourceScope`, `TransactionView`, `Entity`, `PersonBulk`, `SpecialZone`) and
serves `/user/*`, `/admin/*`, `/listTransactions` and `/specialZone/*`.

This is safe because **a session is not server-scoped**:
`atlas_dashboard.registration_token` has no `server_name` column, so one token
authenticates against both application servers (verified — the same token
returns `200` on a driver-app route and a rider-app route). Rider-side operators
authenticate against the same tree; there is no second copy.

rider-app therefore **404s** those four path families. That constrains ingress —
see *Ingress switch* below.

---

## The public contract vs. the "Helper" shape

This explains the bulk of the diff and is the easiest thing to get wrong.

- `provider-dashboard` exposed a **public** API shape to the browser.
- Internally it called a **Helper** variant on the application server, which
  took extra path captures and query parameters that the dashboard filled in
  from the verified session — `fleetOwnerId`, `requestorId`, `volunteerId`, the
  caller's display name, an alert `topic`, and others.

**Serving a route directly means serving the public shape and filling those
values from the session.** Where an app server kept exposing the Helper shape,
the public URL either did not exist (404) or demanded a parameter no caller
sends (400). Both classes were present and are fixed.

Session-derived values are exposed as accessors on `DashboardUser`
(`Tools.Auth.DashboardUser`):

`dashboardRequestorId` · `dashboardRequestorName` · `requestorFleetFlag` ·
`requestorIsFleetOwner` · `requestorHasFleetMemberHierarchy`

---

## Dashboard-side logic that moved

The dashboard was never a pure proxy. Several routes read or wrote
`atlas_dashboard` around the forwarded call. That work now lives in
`Tools.Auth.DashboardUser` and `Tools.Auth.DashboardRegistration` on driver-app.

| Category | Examples |
|---|---|
| Fleet-owner resolution | `getFleetOwnerId`, and the merchant-based variant deciding whether the caller acts as themselves or for an owner |
| Access guards | `checkFleetOwnerVerification`; the `updateFleetOwnerInfo` `AccessDenied` check |
| Session token minting | `/fleet/v2/verify/otp` — the dashboard issued the token; the app response carries none |
| Dashboard person lifecycle | creation on fleet login and operator-creates-fleet; `verified` flag; name/e-mail/role updates; deletion on permanent driver delete |
| Fully dashboard-local routes | `putAccountUpdateRole`, `getAccountFetchUnverifiedAccounts` — these never reached an application server |
| Response reshaping | routes where the dashboard consumed the app response, acted on it, then returned something else |

---

## Building on top of this

Rules for anyone adding or modifying a dashboard endpoint from here on.

**Adding an endpoint to a folder already migrated**

1. Edit the spec YAML as usual and run `, run-generator`. The
   `API/Action/DashboardAuth/…` route is emitted automatically.
2. Register the endpoint id in `capability_endpoint`. Authorisation **fails
   closed** — an unregistered endpoint returns `403`, not open access.
3. Add a seed migration if the id must exist before the deploy.

**Migrating a new spec folder**

- Set `_appServerDashboardAuth = Some True` in that folder's `dsl-config.dhall`.
- This changes the contract the dashboard derives its client from, so the folder
  must be cut over to direct traffic **in the same release**.

**Rules that are easy to violate**

| Rule | Why |
|---|---|
| Serve the **public** shape, never the Helper | The Helper's extra captures/params make the public URL 404 or 400 |
| Take session values from `DashboardUser` accessors, never as a path capture or query param | A caller must not be able to assert who they are |
| Never edit `src-read-only/` | Regenerated; hand edits are silently lost |
| Wrap dashboard-DB access in `runInDashboardDb` | Otherwise it resolves against the application database |
| Keep `provider-dashboard`'s route serving the same path | Both trees stay live so rollback remains a routing flip |

**If your handler needs work the dashboard used to do around the call**
(reading or writing dashboard rows), put it in `Tools.Auth.DashboardUser` or
`Tools.Auth.DashboardRegistration` rather than inlining it into the generated
handler.

**Writes that span both databases have no transaction.** Registration endpoints
create an application person and then a dashboard person; the two databases
cannot be committed together, so a failure between them leaves the application
person orphaned — registered, unable to log in, and blocking its own retry with
`USER_ALREADY_EXISTS`. Narrow that window by validating every *deterministic*
precondition of the dashboard write before the application write, the way
`assertOperatorRegistrable` checks the duplicate contact, the merchant and the
role up front. Only an infrastructure failure should be able to land between the
two.

**Parity checks.** Run these before merging any change to a migrated folder;
each compares the app-side tree against `provider-dashboard`'s public tree:
route coverage · handler signature vs. servant type · call arity and
`Maybe`-ness · response type · query parameters.

---

## Production rollout

### 1. Application config (dhall)

Add a dashboard database block to each application server's config:

```dhall
let esqDashboardDBCfg =
      { connectHost = env:DASHBOARD_DB_HOST ? "localhost"
      , connectPort = env:DASHBOARD_DB_PORT ? 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = env:DASHBOARD_DB_NAME as Text ? "atlas_dev"
      , connectSchemaName = "atlas_dashboard"
      , connectionPoolCount = +10
      }
```

Wired via `esqDashboardDBCfg` / `esqDashboardDBReplicaCfg` in `AppEnv`, both
`Maybe`. Omit them and the server behaves exactly as before.

driver-app additionally needs the login-tree settings:
`authTokenCacheKeyPrefix`, `registrationTokenExpiry`,
`registrationTokenInactivityTimeout`, `passwordExpiryDays`,
`internalAuthAPIKey`, `exotelToken`, and the `twoFa*` / `totp*` group.

### 2. Environment variables

| Variable | Default |
|---|---|
| `DASHBOARD_DB_HOST` / `DASHBOARD_DB_PORT` / `DASHBOARD_DB_NAME` | `localhost` / `5434` / `atlas_dev` |
| `DASHBOARD_DB_REPLICA_HOST` / `DASHBOARD_DB_REPLICA_PORT` | falls back to master |
| `POSTGRES_DASHBOARD_CONNECTION_NAME` | `postgresDashboard` |
| `POSTGRES_DASHBOARD_REPLICA_CONNECTION_NAME` | `postgresDashboardReplica` |

### 3. Database grants — apply before routing any traffic

`dev/ddl-migrations/provider-dashboard/0107-grant-app-servers-dashboard-read.sql`

The application servers connect to `atlas_dashboard` as their **own** database
users, which hold no privileges there. Without this grant every direct request
fails with `permission denied for schema atlas_dashboard` (SQLSTATE 42501).
Two levels are needed:

- **Read + audit** — session verification and audit rows; sufficient for all
  merchant-scoped routes.
- **Write** — the login tree mutates `registration_token`, `person`, `role`,
  `merchant_access`, `person_capability`, `person_resource_access`,
  `deleted_user`, `access_matrix`.

Failure mode is at request time, not at boot.

> **⚠ This migration was renumbered.** It was originally
> `0106-grant-app-servers-dashboard-read.sql` and became `0107-` to make room for
> upstream's `0106-resource-scope.sql`. An environment that already applied it
> under the old number has *that* filename in `schema_migrations`, so the runner
> treats upstream `0106` as unapplied, re-executes it, and aborts with
> `relation "person_resource_access" already exists` — which prevents
> `provider-dashboard` from booting at all. Check
> `SELECT filename FROM atlas_dashboard.schema_migrations WHERE filename LIKE '010%'`
> and reconcile before starting the dashboard.

### 4. Seed migration

`dev/seed-migrations/dashboard-unification/0019-searchtry-endpoint-rename.sql`
— repairs a `capability_endpoint` id left behind by an earlier path rename.
Required because authorisation now fails closed on a missing row.

**Operator role.** `POST /operator/register` resolves the role it assigns by
`dashboard_access_type = 'DASHBOARD_OPERATOR'`. Verify a role with that access
type exists in the target environment's `role` table before routing operator
traffic: production currently carries a role *named* `OPERATOR` whose access
type is `DASHBOARD_USER`, which will not match. `dev/seed-migrations/provider-dashboard/0001-roles.sql`
seeded the same row with the access type `'OPERATOR'`, which is not a
constructor of `DashboardAccessType` at all; that is corrected here.

### 5. Ingress switch

`dev/nginx/nginx.conf` carries the location blocks. The path after the prefix is
identical on both sides, so this is a pure prefix swap:

| Prefix | Target |
|---|---|
| `/bpp/driver-offer/` | driver-app `/direct-dashboard/` |
| `/bap/` | rider-app `/direct-dashboard/` |

`issueManagement` is included in both prefixes — it is served by the application
servers (`issue/config` and `issueV2/config` both return `200`). A comment in
`nginx.conf` excluding it is stale.

**The login tree needs a third, separate rule.** `/admin/*`, `/user/*`,
`/listTransactions` and `/specialZone/*` sit outside both prefixes, so the two
flips above do not move them and they keep reaching `provider-dashboard` — a
valid resting state. To move them, route them to **driver-app specifically**:

| Prefix | Target |
|---|---|
| `/admin/`, `/user/`, `/listTransactions`, `/specialZone/` | driver-app `/direct-dashboard/` |

Do **not** point these at rider-app or balance them across both — rider-app does
not mount the tree and will 404.

control-center needs **no production change** — it calls one host with a path
prefix and the gateway decides the destination. The only frontend change is for
local development, in `control-center/.env`:

```
VITE_DASHBOARD_DIRECT=true
VITE_BPP_DIRECT_URL=http://localhost:8016
VITE_BAP_DIRECT_URL=http://localhost:8013
VITE_DASHBOARD_FALLBACK_URL=http://localhost:8018
```

with the node server using `DASHBOARD_AUTH_URL=http://localhost:8016` and
`DASHBOARD_AUTH_PROFILE_PATH=/direct-dashboard/user/profile`.

### 6. Order of operations

1. Release namma-dsl, then shared-kernel.
2. Deploy the application servers with the dashboard DB config **absent** —
   behaviour is unchanged, nothing is routed yet.
3. Apply the grants migration and the seed migration.
4. Add the dashboard DB config and redeploy.
5. Flip ingress per prefix, one at a time.

**Rollback** at any point is flipping the nginx location back to
`provider-dashboard`. No deploy, no migration reversal.

---

## Verification status

### Live sweep (nginx in front, both dashboards running)

Routes enumerated from the servant types in `src-read-only` — the same source
the servers compile from, so the list cannot drift from the code.

| Class | Count | Meaning |
|---|---:|---|
| OK | 149 | 2xx with a real response |
| REACHED | 719 | handler or auth answered (400/401/403, or a JSON-bodied 404) |
| SERVER_ERROR | 42 | reached the handler, 500 |
| AMBIGUOUS | 24 | router 404 on an enum-typed capture a generic prober cannot satisfy |
| **NOT_MOUNTED** | **0** | — |
| **Total** | **934** | **910 (97.4%) mounted and reachable** |

- **nginx-proxied vs direct-to-app: 0 status mismatches across 855 comparable
  endpoints.** The `token` header survives `proxy_pass` untouched.
- Authorization through the proxy: admin → `200` with data; fleet → `403
  ACCESS_DENIED`; bad or absent token → `401`/`400`.
- Audit rows land in `atlas_dashboard.transaction` in the pre-existing format for
  both action unions.
- **control-center hits the application servers directly.** With both dashboards
  running and reachable as fallback, driver-app received
  `/direct-dashboard/user/profile` and `provider-dashboard` received only `/`
  health probes — zero API traffic.

The 42 server errors were each traced to a cause and **none is a migration
defect**: ~25 are genuinely-absent entities (the prober's placeholder ids), 11
are local schema drift, 2 are stale seed data, 1 is `Not Implemented`, 3 are
external-service failures. Decisive check: every `Storage/Beam` or
`Storage/Queries` file this work touches lives under `app/dashboard/*` — none in
the application-server trees.

### Parity checks

| Check | Result |
|---|---|
| Handler signature vs. servant type | clean |
| Call arity and `Maybe`-ness vs. domain function | clean |
| Response type vs. dashboard | clean |
| Query parameters vs. dashboard | clean |
| Dashboard-DB logic ported | complete |

**Not yet verified:** the `SPECIAL_ZONE_*` and `PROVIDER_FLEET` route families
(no working path found locally); KV buffering of `transaction` writes (no
`atlas_dashboard` table is KV-backed in the local stack, so `createWithKV`
write-throughs go straight to Postgres and the path cannot be exercised there);
and business-level payload equivalence against the dashboard's own responses.

Functionally exercised with `provider-dashboard` stopped: login, the full
administration tree, and both platforms' ops routes.

**These checks verify shape, not semantics.** They cannot tell whether a
resolved `fleetOwnerId` is the *correct* one, or whether a ported guard still
denies what it should. The four flows carrying genuinely ported logic were
therefore exercised end-to-end against a live stack with both databases:

| Flow | Result |
|---|---|
| Fleet-owner login (token minting) | dashboard person created with the app person's id; token minted and authenticates |
| Fleet registration, and operator-creates-fleet | placeholder name/email replaced on the dashboard person; operator-created fleet lands in both databases under one id |
| A fleet **member** acting for an owner | resolves to the associated owner; an unassociated `fleetOwnerId` is refused with `ACCESS_DENIED` |
| Permanent driver delete | `person`, `merchant_access` and `registration_token` all removed from the dashboard database |

---

