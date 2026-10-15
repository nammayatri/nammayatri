# Dashboard Auth — one token per person (check at handler entry)

## Goal
Move `atlas_dashboard.registration_token` from per-(personId, merchantId, operatingCity) to **one token
per person**. For authorization the token resolves to `personId` only; the merchant + city a request
acts on come from the **URL** (direct-dashboard) or the **request body / the person's home merchant**
(legacy tree), and authorization = the person has a `merchant_access` row for that (merchant, city),
matched against their full set cached in Redis (no `merchant_pair` hop; SUPER_ADMIN bypasses).

Scope note: the primary target is the **direct-dashboard** tree, but the legacy `DashboardAuth`
(`TokenInfo`) tree is also reworked to stop depending on the token's merchant/city (point 5). Only
unified-dashboard's static token and the rider/driver business logic are left alone.

## Shared changes
1. **Schema / type.** Drop `merchant_id`, `operating_city` from the Haskell `RegistrationToken`
   (type + Beam + queries; the per-(person,merchant,city) query family is removed).
   **DB migration is rolling-deploy safe, not a drop:**
   `dev/ddl-migrations/dashboard/0112-relax-notnull-merchant-city-on-registration-token.sql` only does
   `ALTER COLUMN merchant_id / operating_city DROP NOT NULL` so the new identity-only INSERT (which omits
   them) doesn't violate the constraint. The columns stay in the DB (nullable) so pre-refactor pods keep
   working during coexistence. The destructive `DROP COLUMN` is a **phase-2 follow-up migration**, run
   only after old pods drain. (No dedupe / `UNIQUE(person_id)` step.)
2. **Token resolution** (`Lib/src/Tools/Auth/Common.hs`) — `verifyPerson :: RegToken -> m (Id Person)`;
   the auth cache value is just `personId`; the per-request `merchant_access` re-check is dropped from
   `validateToken` (authz is now per request); only `cleanCachedTokens personId` is kept.
3. **Access cache** (new `Lib/src/Tools/Auth/Scope.hs`) — `resolveMerchantAccess` reads the person's
   `merchant_access` set (`findAllMerchantAccessByPersonId`) through a Redis cache `dsh:macc:<personId>`
   (TTL ~600s). `invalidateMerchantAccess personId` is called at the `merchant_access` grant/revoke +
   person-delete sites in **`Person.hs`** (assign / reset / resetCity / deletePerson / bulkUpsert). It is
   **not** called in `Merchant.hs`/`Registration.hs` — those create brand-new persons with no cache entry
   yet, so invalidation there is a no-op.
4. **Login** (`Lib/src/Domain/Action/Dashboard/Registration.hs`) — `generateToken` mints exactly one
   token per person (wipes existing first); `LoginRes` still returns `merchantId`/`city` as the landing
   MOC; `logout`/`logoutAllMerchants` delete the person's token (person-scoped) and audit against the
   person's home merchant.
5. **Legacy `DashboardAuth`** — `TokenInfo` becomes person-only (`{personId}`); the ~32 readers in
   `Domain/Action/Dashboard/{ResourceScope,Person,Merchant,Registration,EmailVerification}.hs` + driver
   `API/DashboardDriverInfoByPhoneNumber.hs` are reworked to get the merchant/city from the **request
   body** or the person's **home merchant** (`person.merchantId`), with the authz flipped to
   "caller has `merchant_access` for the target merchant" (`assertMayGrantAccessToMerchant` in `Person.hs`,
   `assertAdminMayManageMOC` in `ResourceScope.hs`), else SUPER_ADMIN. Audit rows stamp from
   `person.merchantId`.

## Where the merchant/city check runs — at handler entry (typed captures)
Merchant and city are the handler's typed `Capture` args, and the operator comes from the
`DashboardUser` the combinator produced. The check is the first statement of each generated handler.

- `Lib/src/Tools/Auth/Scope.hs` — `enforceMerchantCityAccess :: Id Person -> Text -> City -> m ()`
  (merchant short-id as **`Text`**, so the lib needn't share the app's `Merchant` phantom): load the
  cached set; unless SUPER_ADMIN, require the URL's (merchant, city) in it, else `AccessDenied`.
- Per-app wrapper `enforceMerchantCityAccessForUser :: DashboardUser -> ShortId b -> City -> m ()` in each
  app's `Main/src/Tools/Auth/DashboardUser.hs` (re-exported via `Tools.Auth.DashboardUserAuth`). It
  **must** wrap the call in `runInDashboardDb` — `isSuperAdmin` and the `merchant_access` read hit
  `atlas_dashboard`, a separate DB connection from the app's own.
- **Generator (namma-dsl, `NammaDSL/Generator/Haskell/Servant.hs`)** — emits
  `Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a<user> a<merchant> a<city>`
  (the wrapper, passed the whole `DashboardUser` + the typed merchant/city captures) as the first
  handler-body statement for every `DashboardUserAuth` DASHBOARD endpoint — same slot as
  `auditDashboardAction`. No YAML/spec change. Requires an upstream namma-dsl commit + `flake.lock`
  bump + regenerate `src-read-only/API/Action/DashboardAuth/**`.
- The combinator (`Verify.hs`) is **not** slimmed to person-only. `verifyUrlScope` is removed, but
  `verifyDashboardUser` still calls `resolveScopeFromUrl`, which resolves merchant+city **from the URL**
  (keeping `verifyCity` — merchant supports city, and the server-name check), builds the `DashboardUser`
  with those, then runs capability (Layer A) + resource-scope (Layer C) against that MOC. So
  `DashboardUser` keeps `{personId, merchant, city, person}` (merchant/city now sourced from the URL, not
  the token). `verifyServerWithPair`/`verifyServer` remain exported but unused by this path.
  `verifyDashboardUserAction` signature + its `App/Server.hs` `Context` entry are unchanged.

Authentication + capability still reject **before** the body; the merchant/city check runs after body
parse (same spot as `auditDashboardAction`).

## Additional fixes made during review
- **C1 — dashboard-DB context:** `enforceMerchantCityAccessForUser` wraps the check in
  `runInDashboardDb` (both apps). Without it the `merchant_access` read lands on the app DB, where the
  table doesn't exist → every non-super-admin direct-dashboard call fails on a cold cache. (Not caught
  locally because locally both schemas share one DB.)
- **`changeEnabledStatus` tenant guard:** addressed by `personId` with no merchant in the request; the
  old per-MOC token update made cross-merchant calls an inert no-op, which the identity token removes, so
  it now asserts `assertMayGrantAccessToMerchant` against the **target's home merchant** (SUPER_ADMIN
  bypasses).
- **Completeness / stale generation:** every `DashboardUserAuth` handler taking `DashboardUser` +
  merchant + city must carry the enforce call. Modules generated before the emission feature (incl. new
  `Payout`/`Merchant` endpoints, `IssueManagement`, `IncentiveJourney`, `RideFeedback`, `FeedbackForm`)
  were regenerated to add it. `Backend/dev/check-dashboard-enforce.sh` is a CI gate that fails the build
  if any handler is missing it.

## Rollout (expand-contract, rolling-deploy safe)
- **Phase 1 (this release):** identity-only auth; migration relaxes `NOT NULL` only (columns retained).
- **Phase 2 (follow-up, after old pods drain):** drop `merchant_id` / `operating_city`.
- Residual during phase 1 (no dual-write): a token minted by a new pod has NULL merchant/city; an old pod
  reading that token can error until rollout completes. Eliminating that fully needs dual-write or
  draining old pods first.
