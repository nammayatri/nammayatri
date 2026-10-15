# Dashboard Auth — One Token Per Person

## The idea

Earlier, a dashboard operator got a **separate login token for each merchant + city** they
worked in, and the token itself carried that merchant/city as its scope.

Now each person has **one identity-only token**. The token proves *who you are* — nothing
more. Which merchant/city a request acts on comes from the **request** (the URL or the body),
and authorization is a single question answered fresh on every call:

> Does this person have a `merchant_access` row for the merchant + city this request is touching?
> (Super-admins bypass and are allowed everywhere.)

Mental model: **token = who you are · request = what you're touching · `merchant_access` = whether that's allowed.**

## Request flow

1. **Login** (email+password or phone+OTP) issues one identity token and wipes any previous
   tokens for that person — exactly one token per person.
2. On each request, the system figures out the **merchant + city** being acted on (see below),
   not from the token.
3. Before the real work runs, it checks the caller's `merchant_access` for that merchant+city
   (cached in Redis at `dsh:macc:<personId>`, TTL 600s; SUPER_ADMIN bypasses). Allowed → proceed;
   not allowed → `403 ACCESS_DENIED`.
4. Because access is decided per-request, the single token reaches **every** merchant/city the
   person is provisioned for — just by changing the request. No re-login, no token switching.

All these reads hit the **dashboard database** (`atlas_dashboard`), which is a *separate* DB
connection from the app's own, so they must run inside `runInDashboardDb`.

## Two kinds of endpoints

### 1. Direct-dashboard APIs (`/direct-dashboard/{merchantShortId}/{city}/…`)

Merchant + city come from the **URL**. The access check is **auto-injected** as the first
statement of every handler by the code generator — no hand-written check per endpoint.

| Piece | Where |
|---|---|
| The actual check (`merchant_access` lookup + SUPER_ADMIN bypass) | `Backend/app/dashboard/Lib/src/Tools/Auth/Scope.hs` → `enforceMerchantCityAccess` |
| Per-app wrapper (adds `runInDashboardDb`, takes the operator + typed `Capture`s) | `…/dynamic-offer-driver-app/Main/src/Tools/Auth/DashboardUser.hs`, `…/rider-app/Main/src/Tools/Auth/DashboardUser.hs` → `enforceMerchantCityAccessForUser` (re-exported via `Tools.Auth.DashboardUserAuth`) |
| The injected call (one per handler) | generated into `…/src-read-only/API/Action/DashboardAuth/**/*.hs` by NammaDSL |
| URL → merchant/city resolution + capability (Layer A) + resource-scope (Layer C) | `Backend/app/dashboard/Lib/src/Tools/Auth/Verify.hs` → `verifyDashboardUser`, `resolveScopeFromUrl` |

### 2. Legacy "DashboardLogin" APIs (login, logout, person/merchant/role/resource-scope mgmt…)

No `/{merchant}/{city}/` URL, so there's no URL to read and **no generated guard**. The auth
combinator resolves identity only; each endpoint's access check is **hand-written inline** in its
domain action. These split into three shapes:

- **Identity/session endpoints** (`login`, `logout`, `getCurrentMerchant`, `profile`,
  `changePassword`, 2FA, email-verify): only need `personId`; where they used to lean on the
  token's merchant/city they now use the person's **home merchant** (`person.merchantId`).
- **"Act on a merchant named in the body"** (`assignMerchantCityAccess`, `resetMerchantAccess`,
  resource-scope assign/reset, `createPerson`, `createUserForMerchant`, `changeEnabledStatus`):
  merchant/city come from the **request body** (or the target's home merchant); the guard is
  **"does the caller have access to that merchant?"** (else SUPER_ADMIN).
- **"Show me what I can see" reads** (`getUserResourceScope`, `getPersonResourceAccess`): return
  results across the caller's **full `merchant_access` set**.

| Piece | Where |
|---|---|
| Combinator → identity-only `TokenInfo` + capability/tier check | `Backend/app/dashboard/Lib/src/Tools/Auth/Dashboard.hs` → `verifyDashboard` |
| Caller↔target merchant guard (grant/reset access) | `…/Domain/Action/Dashboard/Person.hs` → `assertMayGrantAccessToMerchant` |
| Caller↔target MOC guard (resource scope) | `…/Domain/Action/Dashboard/ResourceScope.hs` → `assertAdminMayManageMOC` |
| createPerson / createUserForMerchant / changeEnabledStatus / home-merchant logic | inline in `Person.hs`, `Merchant.hs`, `Registration.hs` |

Both inline guards and the generated direct-dashboard check ultimately call the same primitives —
`QAccess.findByPersonIdAndMerchantId[AndCity]` (`Storage/Queries/MerchantAccess.hs`) +
`Capability.isSuperAdmin` — so the rule is identical everywhere; only *where the merchant/city comes
from* differs (URL vs request body / home merchant).

## Guardrail

Every direct-dashboard handler that takes a `DashboardUser` + merchant + city **must** carry the
enforce call. `Backend/dev/check-dashboard-enforce.sh` fails the build if any one doesn't — so a
new spec/endpoint can't silently ship without the guard. The fix for a violation is in the
generator + regenerate (never hand-edit `src-read-only/`).

## Rollout (expand-contract, rolling-deploy safe)

The token's `merchant_id` / `operating_city` columns are **not dropped in this release**:

- **Phase 1 (this release):** new code stops using those columns for auth, and the migration only
  **relaxes `NOT NULL`** (`0112-relax-notnull-…`) so new inserts (which omit them) don't violate the
  constraint. Columns stay in place so pre-refactor pods keep working during coexistence.
- **Phase 2 (follow-up, after old pods drain):** a later migration **drops** the columns.

> Residual note: with phase-1 only (no dual-write), a token minted by a *new* pod has NULL
> merchant/city; an *old* pod reading that token can error until the rollout completes. Eliminating
> even that window would require dual-writing the columns, or draining old pods first.
