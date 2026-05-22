# Chennai Metro SVP — Validation API Plan

## 1. Context

Chennai Metro (CMRL) station gates need to call our rider-app backend in real time when a passenger scans a Stored Value Pass (SVP) QR. We are the issuer (Chennai One / CUMTA side, per Doc 2). The gate extracts an identifier from the QR, POSTs a signed request to us, and we authorize the entry / exit. The gate opens or denies based on our verdict; on EXIT we debit the user's wallet for fare + overstay penalty.

This plan covers exactly **one endpoint**: `POST /external/cmrl/validate-svp`. The corresponding `/sync-svp`, `/sync-svp-all`, the SVP purchase + recharge flow, stale-entry recovery, and decommission of the legacy `/svp/*` module are explicit follow-ups — out of scope here.

SVP integrates into the existing pass infrastructure: it's a new `PassEnum` variant; each rider has a `PurchasedPass` row carrying SVP-specific lifecycle state; balance lives in the user's Juspay wallet (the same store the legacy `/svp/gate` already uses); every gate scan writes to the existing `PassVerifyTransaction` audit table (shared with the bus verify flow).

---

## 2. Wire contract

```
POST /external/cmrl/validate-svp

Headers (SHA256-EdDSA, per Doc 2):
  Digest:           SHA-256=<base64(SHA-256(body))>
  X-Signature-Alg:  SHA256-EdDSA
  X-Timestamp:      <UTC ISO 8601, e.g. 2026-05-21T08:00:00Z>
  X-Signature-Kid:  <CMRL's current signing key id>
  X-Signature:      <base64 Ed25519 signature over the canonical signing string>

Canonical signing string  (newline-separated, no trailing whitespace,
header names lowercased):
  POST /external/cmrl/validate-svp
  host: <Host header>
  x-timestamp: <X-Timestamp>
  digest: <Digest>

Body:
  { "stationCode": "SMM",
    "passNumber":  9000001,                      // CDAC TAG 83 Ticket Serial No (Int64, 8 bytes)
    "scanType":    "ENTRY" | "EXIT" }

200 OK:
  { "result": "Success" }

4xx / 5xx:
  { "errorCode":    "INVALID_PASS" | ...,
    "errorMessage": "<human-readable>" }
```

`passNumber` is an integer because CDAC TAG 83 Ticket Serial No is 8 bytes — `Int64` fits exactly. It maps directly to the existing `PurchasedPass.passNumber` column (already a `SecondaryKey` with an auto-generated `findByPassNumber` query — no new lookup columns needed). The `PurchasedPass.id` UUID stays internal; the gate never sees it.

---

## 3. Locked architectural decisions

1. **Endpoint URL**: `POST /external/cmrl/validate-svp`. New `/external/cmrl/*` subtree, distinct from rider-facing `/multimodal/pass/{id}/verify` because the auth model is entirely different (EdDSA signature from a gate vs. `TokenAuth` from a rider phone). Future `/sync-svp` lives next door.

2. **Body identifier `passNumber :: Int` (Int64)** — wire field name matches the existing `PurchasedPass.passNumber` column directly. CDAC-strict (TAG 83 Ticket Serial No, 8 bytes). Lookup uses the auto-generated `findByPassNumber` SecondaryKey query — no new columns, no new indexes.

3. **Request authentication: SHA256-EdDSA** per Doc 2. Custom Servant combinator `EdDsaSignatureAuth` modelled on the existing `Tools/SignatureAuth.hs` (RSA variant). Pure verifier underneath the `HasServer` instance for unit testing. From the spike: `addAuthCheck` requires `type ServerT (EdDsaSignatureAuth :> api) m = UTCTime -> ServerT api m` — the combinator parses `X-Timestamp` once for the freshness check and forwards the parsed `UTCTime` to the handler so all downstream time math uses the **gate's clock**.

4. **Per-merchant key resolution.** Kid + Ed25519 public key live as two `Maybe` fields on the existing `CMRLConfig` record (per merchant-operating-city, in `integrated_bpp_config.provider_config` JSON). At server startup the resolver scans every loaded CMRL config and builds an in-memory `Map kid → (publicKey, merchantId, cityId)` for O(1) per-request lookup. When Doc 2 Phase 2 key-rotation arrives, we promote these two fields to a dedicated `cmrl_gate_key` table — straightforward future migration; not needed for the initial integration with one keypair.

5. **Audit table = existing `PassVerifyTransaction`** with four new columns:
   - `scanType :: Maybe ScanType` — `ENTRY | EXIT`
   - `fareCharged :: Maybe HighPrecMoney`, `penaltyCharged :: Maybe HighPrecMoney` — stored separately so analytics can isolate overstay frequency
   - `tripStatus :: Maybe TripStatus` — `ACTIVE | EXITED | CANCELLED` (no `TIMED_OUT` yet — that lives in the deferred stale-entry plan). Set `ACTIVE` on the ENTRY row; transitioned to `EXITED` on successful EXIT, or `CANCELLED` on same-station within-threshold cancellation.

   Two rows per SVP trip (ENTRY then EXIT). The ENTRY row's `tripStatus` is mutated as the trip closes; the EXIT row is append-only. Lookup is `findActiveByPurchasePassId pp.id ACTIVE` — single query, indexed `(purchase_pass_id, trip_status)`.

   No `tripId` column for now. Live mapping (the ACTIVE entry at EXIT time IS the entry for this exit) is sufficient. If historical pair reconstruction is needed later, `tripId` can be added without breaking anything.

6. **`fleet_id` and `valid_till` on `PassVerifyTransaction` become `Maybe`** (`DROP NOT NULL` migrations). Bus handler wraps existing values in `Just`; SVP handler writes `Nothing`.

7. **Pass validity = 5 years from last recharge.** The purchase + recharge plan enforces this (updates `startDate` and `endDate`). This endpoint just checks `today > endDate`. Mentioned here so the purchase plan reuses consistent fields.

8. **No `passBalance` shadow column.** Balance reads + debits go directly to the user's wallet via the existing helper that the legacy `/svp/gate` uses. One source of truth.

9. **Real CMRL fare lookup on EXIT** via `CallAPI.getFares` with `Just SVP` as `ticketTypeId` (Doc 1 §2.2 product 103). Same-station case short-circuits to `0`. `Just SVP` is flagged as a temporary value — proper CMRL ticket-type mapping needs Task 0 verification.

10. **Overstay penalty fully implemented**, config-driven. Same threshold (`cmrlSvpOverstaySameStationMins`, default 20 min) doubles as the **same-station free-cancellation window** — entries-exits within this window at the same station are treated as cancellations (no fare, no penalty). This matches CMRL's published policy: same-station < 20 min → no penalty; ≥ 20 min → Rs.10/h penalty up to Rs.100 cap.

11. **Error envelope** via the existing nammayatri pipeline. New `CmrlGateError` ADT in `Tools/Error.hs` with `IsBaseError` / `IsHTTPError` / `IsAPIError` instances. The standard `apiHandler` → `throwServantError` machinery serialises it as Doc 2's `{ errorCode, errorMessage }` automatically.

12. **No NammaDSL for the API spec.** Custom auth combinator isn't supported by the current generator. The route is hand-written under `src/API/Action/External/CMRL.hs`. Storage YAML still flows through NammaDSL.

13. **Handler structured as pure-core + IO-shell.** Record-of-functions `Deps` so tests inject in-memory fakes. Overstay penalty is a pure function for exhaustive unit testing.

14. **`merchantId` / `merchantOperatingCityId` come from `PurchasedPass`**, not the gate context. Simplest source; correct for single-CMRL deployment. Cross-merchant cross-check (via the gate-key row) is a future enhancement when multi-metro support arrives.

15. **Stale-entry auto-close + sweeper still deferred** — but the ENTRY-side error contract now distinguishes the two scenarios: a same-station re-tap within 20 min returns 409 `DUPLICATE_ENTRY` ("you just entered, duplicate scan"); any other "active trip exists" condition (different station, or same-station after 20 min) returns 409 `NO_EXIT_FOUND` ("Previous trip incomplete — visit EFO"). Eventual sweeper / auto-close logic plugs in behind that contract.

16. **Full Hspec coverage** — verifier (10 branches), handler (~16 branches), pure penalty function (~6 cases).

---

## 4. Configs

### 4a. New optional fields on existing `CMRLConfig`

```haskell
-- gate request-signature verification
cmrlGateSigningKid             :: Maybe Text            -- expected X-Signature-Kid for this merchant's gates
cmrlGateSigningKey             :: Maybe Base64          -- Ed25519 public key, raw 32 bytes, base64
cmrlGateClockSkewSeconds       :: Maybe Int             -- default 300  — signature replay window (seconds)

-- SVP entry policy
cmrlSvpEntryMinBalanceRupees   :: Maybe HighPrecMoney   -- default 50   — min wallet balance required at ENTRY

-- SVP overstay penalty policy (matches CMRL's published policy)
cmrlSvpOverstaySameStationMins :: Maybe Int             -- default 20   — same-station: <T = cancellation, ≥T = overstay penalty applies
cmrlSvpOverstayDiffStationMins :: Maybe Int             -- default 120  — different-station: ≥T = overstay penalty applies
cmrlSvpOverstayPerHourRupees   :: Maybe HighPrecMoney   -- default 10   — Rs per started hour past threshold
cmrlSvpOverstayMaxRupees       :: Maybe HighPrecMoney   -- default 100  — penalty cap
```

All `Maybe` so existing `integrated_bpp_config` JSON keeps deserialising unchanged. Defaults applied in the handler if `Nothing`.

### 4b. Summary

- **8 new fields** on existing `CMRLConfig` (per merchant-operating-city, in `provider_config` JSON)
- **Zero new tables** — Phase 1 kid + key live alongside the rest of the merchant's CMRL config. Phase 2 key-rotation (Doc 2) can promote these into a dedicated `cmrl_gate_key` table later if multi-kid support becomes a real need.
- **Zero env vars**, zero shared-kernel changes, zero global config rows

---

## 5. Schema changes

### 5a. `spec/Storage/PassType.yaml`

Append `, SVP` to `PassEnum` enum string. Stored as `TEXT` — no migration needed for the enum itself.

### 5b. `spec/Storage/PurchasedPass.yaml`

Inside the embedded `PassVerifyTransaction` block:
- Add `ScanType` enum (`ENTRY, EXIT`) under `types:` with derive + domainInstance for Beam + HTTP.
- Add `TripStatus` enum (`ACTIVE, EXITED, CANCELLED`) under `types:` with same derivations.
- Change `validTill: UTCTime` → `Maybe UTCTime`, `fleetId: Text` → `Maybe Text`.
- Append fields: `scanType: Maybe ScanType`, `fareCharged: Maybe HighPrecMoney`, `penaltyCharged: Maybe HighPrecMoney`, `tripStatus: Maybe TripStatus`.
- Add to `sqlType:`: `scanType: text`, `tripStatus: text`.
- Add `default:` block: `fleetId: "NULL"`, `validTill: "NULL"` (so NammaDSL emits `SET DEFAULT NULL` alongside `DROP NOT NULL`).
- Add two new YAML queries:
  ```yaml
  findActiveByPurchasePassId:
    kvFunction: findOneWithKV
    where:
      and: [purchasePassId, tripStatus]
  updateTripStatusOfActive:
    kvFunction: updateOneWithKV
    params: [tripStatus]
    where:
      and: [purchasePassId, tripStatus]
  ```

After regen:
- Migration is purely additive on `PurchasedPass` (no new columns required there for this plan).
- Migration on `PassVerifyTransaction`: `DROP NOT NULL` for `fleet_id` + `valid_till`, `ADD COLUMN` for `scan_type`, `fare_charged`, `penalty_charged`, `trip_status`.
- Existing record literals in `src/Domain/Action/UI/Pass.hs` need patching: wrap `validTill` and `fleetId` writes in `Just`; add `scanType = Nothing`, `fareCharged = Nothing`, `penaltyCharged = Nothing`, `tripStatus = Nothing`.
- Update `findLastVerifiedVehicleNumberByPurchasePassId` in `PassVerifyTransactionExtra.hs`: change `validTill` filter to `Se.GreaterThan (Just now)`; pattern-match `Just fid <- transaction.fleetId` to skip rows with `Nothing` fleetId.

---

## 6. Signature combinator (`Tools.EdDsaSignatureAuth`)

Two parts:

**Pure verifier** (test-friendly):

```haskell
data VerifyInput    = VerifyInput { viMethod, viPath, viHost, viBody,
                                    viDigest, viAlgo, viTimestamp,
                                    viKid, viSignature :: ByteString }
data VerifierConfig = VerifierConfig { vcExpectedKid :: ByteString
                                     , vcPublicKey   :: Ed.PublicKey
                                     , vcSkewSeconds :: Int }
data VerifyError    = BadDigest | StaleTimestamp | UnknownKid | WrongAlgorithm
                    | MalformedTimestamp | MalformedSignature | BadSignature
                    | MissingHeaderE Text
data VerifyResult   = Ok | Err VerifyError

verifyRequest :: VerifierConfig -> UTCTime -> VerifyInput -> VerifyResult
```

Sequence: missing-header check → algo check → kid check → timestamp parse → skew check → digest recompute & match → base64-decode signature → `Ed25519.signature` parse → canonical signing string → `Ed25519.verify`.

**Servant combinator**: `data EdDsaSignatureAuth` with `HasServer` instance requiring `HasContextEntry ctx VerifierConfigResolver`. `type ServerT (EdDsaSignatureAuth :> api) m = UTCTime -> ServerT api m`. `authCheck :: DelayedIO UTCTime` reads raw body via `Wai.strictRequestBody`, extracts five headers, calls the resolver, runs `verifyRequest`. On success returns the parsed `gateTimestamp`. On `Err` or `Nothing` from the resolver → `delayedFailFatal` with a JSON `ServerError` carrying the Doc 2 error envelope.

**Resolver**: `newtype VerifierConfigResolver = VerifierConfigResolver { resolve :: Wai.Request -> IO (Maybe VerifierConfig) }`. Built **once at startup** in `App/Server.hs` from an in-memory `Map kid → (publicKey, merchantId, cityId)`. The map is constructed by scanning every loaded CMRL `IntegratedBPPConfig` and collecting `(cmrlGateSigningKid, cmrlGateSigningKey)` pairs where both are `Just`. At request time the resolver extracts `X-Signature-Kid` from the WAI request, does a map lookup, and either returns the matching `VerifierConfig` or `Nothing` (→ combinator emits `401 UNKNOWN_KID`). No DB hit per request — pure in-memory lookup. Reload triggers: server restart, or a small admin endpoint (out of scope) that re-runs the scan after a config row update.

---

## 7. Validation flow — ENTRY

Each step says what's checked, what happens on success, what error is thrown on failure.

### Signature layer (combinator)

| # | Check | Failure |
|---|---|---|
| 1 | All five required headers present | 400 `MISSING_HEADER` |
| 2 | `X-Signature-Alg == "SHA256-EdDSA"` | 400 `BAD_ALGORITHM` |
| 3 | `X-Timestamp` parses as ISO 8601 UTC | 400 `BAD_TIMESTAMP` |
| 4 | `\|server_now - X-Timestamp\| ≤ cmrlGateClockSkewSeconds` (default 300) | 401 `STALE_TIMESTAMP` |
| 5 | Resolver's in-memory map (built from loaded CMRL configs) contains the kid | 401 `UNKNOWN_KID` |
| 6 | `Digest` recomputed from body matches header | 400 `BAD_DIGEST` |
| 7 | Ed25519 signature verifies over canonical signing string | 401 `INVALID_SIGNATURE` |

On success the combinator passes `gateTimestamp :: UTCTime` (the parsed `X-Timestamp`) to the handler. All handler time math uses this — never `getCurrentTime`.

### Handler layer (ENTRY-specific)

| # | Check | Failure |
|---|---|---|
| 8 | `findByPassNumber req.passNumber` returns a row (SecondaryKey lookup on the existing `PurchasedPass.passNumber` column) | 404 `INVALID_PASS` |
| 9 | `pp.passEnum == Just SVP` | 404 `INVALID_PASS` (not an SVP) |
| 10 | `utctDay gateTimestamp <= pp.endDate` | 401 `EXPIRED_PASS` |
| 11a | If `findActiveByPurchasePassId pp.id ACTIVE` returns an open trip, branch by station + age: | (see below) |
| 11b | open trip exists, **same station AND** `(gateTimestamp - entryRow.verifiedAt) < cmrlSvpOverstaySameStationMins` (default 20 min) | 409 `DUPLICATE_ENTRY` (passenger has just entered at this station — duplicate scan) |
| 11c | open trip exists, **different station OR same-station beyond 20 min** | 409 `NO_EXIT_FOUND` (passenger has an unclosed prior trip — visit EFO) |
| 11d | no open trip exists | proceed |
| 12 | `dWalletBalance pp.personId >= cmrlSvpEntryMinBalanceRupees` (default Rs.50) | 402 `INSUFFICIENT_BALANCE` (wallet untouched) |

On success:

13. INSERT new PVT row:
    ```
    purchasePassId      = pp.id
    verifiedAt          = gateTimestamp
    scanType            = Just ENTRY
    sourceStopCode      = Just req.stationCode
    fareCharged         = Nothing
    penaltyCharged      = Nothing
    tripStatus          = Just ACTIVE
    passEnum            = Just SVP
    isActuallyValid     = Just True
    fleetId             = Nothing                  -- bus-only
    validTill           = Nothing                  -- bus-only
    merchantId          = Just pp.merchantId
    merchantOperatingCityId = Just pp.merchantOperatingCityId
    createdAt, updatedAt = gateTimestamp
    ```
14. Return `{ "result": "Success" }`.

### ENTRY side effects

- **0 wallet movement** (read-only pre-check at step 12).
- **1 DB write** (the new PVT row).

### ENTRY error short-circuits (no DB writes, no wallet movement)

Steps 1–12 above. Each returns `{ errorCode, errorMessage }` with the appropriate HTTP status.

---

## 8. Validation flow — EXIT

### Signature layer (combinator)

Identical to ENTRY steps 1–7. Same combinator, same checks, same `gateTimestamp` plumbed to the handler.

### Handler layer (EXIT-specific). All time math uses `gateTimestamp`.

#### Resolve the pass

| # | Check | Failure |
|---|---|---|
| 8 | `findByPassNumber req.passNumber` returns a row (SecondaryKey lookup on the existing `PurchasedPass.passNumber` column) | 404 `INVALID_PASS` |
| 9 | `pp.passEnum == Just SVP` | 404 `INVALID_PASS` |
| 10 | `utctDay gateTimestamp <= pp.endDate` | 401 `EXPIRED_PASS` |

#### Find the open trip

| # | Check | Failure |
|---|---|---|
| 11 | `findActiveByPurchasePassId pp.id ACTIVE` returns a row (the open ENTRY) | 409 `NO_ENTRY_FOUND` |
| 12 | That row's `sourceStopCode /= Nothing` | 409 `NO_ENTRY_FOUND` (defensive) |

#### Same-station cancellation short-circuit

| # | Check | Result |
|---|---|---|
| 13 | `entryStation == req.stationCode` AND `(gateTimestamp - entryRow.verifiedAt) < cmrlSvpOverstaySameStationMins` (default 20 min) | Wrap in `runInTransaction`: INSERT CANCELLED PVT row `{ scanType = Just EXIT, sourceStopCode = Nothing, destinationStopCode = Just req.stationCode, fareCharged = Just 0, penaltyCharged = Just 0, tripStatus = Just CANCELLED, ... }` + `updateTripStatusOfActive pp.id ACTIVE → CANCELLED`. No wallet movement. Return Success. |

#### Load merchant policy

| # | Check | Failure |
|---|---|---|
| 14 | `findCMRLConfig pp.merchantId pp.merchantOperatingCityId` returns a row | 500 `GATE_CONFIG_MISSING` |

#### Compute money

15. Let `isSame = entryStation == req.stationCode`.
    - If `isSame` (past cancellation window) → `fare = 0`.
    - Else → `fare = CallAPI.getFares pp.merchantId pp.merchantOperatingCityId entryStation req.stationCode (Just SVP)`.
    *(Note: `Just SVP` is temporary — proper CMRL ticket-type mapping is a follow-up after Task 0 verification.)*

16. Compute overstay penalty (pure function):
    ```
    threshold = 60 * (if isSame then cmrlSvpOverstaySameStationMins      -- default 20
                                else cmrlSvpOverstayDiffStationMins)     -- default 120
    duration  = diffUTCTime gateTimestamp entryRow.verifiedAt          -- seconds
    hoursOver = ceiling (max 0 (duration - threshold) / 3600)
    perHour   = fromMaybe 10  cmrlSvpOverstayPerHourRupees
    capRs     = fromMaybe 100 cmrlSvpOverstayMaxRupees
    penalty   = min capRs (perHour * fromIntegral hoursOver)
    ```

17. `let totalDue = fare + penalty`.

#### Wallet pre-check & debit

| # | Check | Failure |
|---|---|---|
| 18 | `dWalletBalance pp.personId >= totalDue` | 402 `INSUFFICIENT_BALANCE` (wallet untouched, no PVT row) |
| 19 | `dDebitWallet pp.personId totalDue` returns successfully (any raise caught → re-throw) | 502 `WALLET_DEBIT_FAILED` (no PVT row) |

#### Audit write (atomic)

20. Within `runInTransaction`:
    - INSERT EXIT PVT row with `scanType = Just EXIT`, `verifiedAt = gateTimestamp`, `sourceStopCode = Nothing`, `destinationStopCode = Just req.stationCode`, `fareCharged = Just fare`, `penaltyCharged = Just penalty`, `tripStatus = Just EXITED`, plus the SVP-common defaults (`passEnum = Just SVP`, `fleetId = Nothing`, `validTill = Nothing`, etc.). *(The ENTRY row's `sourceStopCode` carries the entry station; the EXIT row's `destinationStopCode` carries the exit station — one stop per row, semantically correct.)*
    - `updateTripStatusOfActive pp.id ACTIVE → EXITED` (close the original ENTRY row)

21. Return `{ "result": "Success" }`.

### EXIT side-effect order (matters for failure recovery)

```
Step 18: wallet read          → cheap pre-check, no money moves
Step 19: wallet DEBIT         → only if pre-check passed
Step 20: DB writes (txn)      → only if debit succeeded; atomic via runInTransaction
```

**Known small drift risk**: if step 20's transaction commit fails *after* step 19's debit succeeded, the user is charged but no EXIT row exists. Mitigation: structured log on this branch + ops monitoring. Worth a follow-up plan (e.g. write-pending → debit → confirm-pending pattern) but not blocking for the initial integration.

### Two success paths

| Path | Wallet | PVT writes | Net result |
|---|---|---|---|
| Step 13 — same-station cancellation | untouched | 1 CANCELLED row + 1 UPDATE entry → CANCELLED | passenger leaves, no charge |
| Steps 14–21 — real exit | debited by `fare + penalty` | 1 EXIT row + 1 UPDATE entry → EXITED | passenger leaves, charged |

### EXIT error short-circuits (no DB writes, no wallet movement)

| Step | Error | HTTP |
|---|---|---|
| 1–7 | Combinator auth failures | 400 / 401 |
| 8 | `INVALID_PASS` | 404 |
| 9 | `INVALID_PASS` (not SVP) | 404 |
| 10 | `EXPIRED_PASS` | 401 |
| 11 | `NO_ENTRY_FOUND` | 409 |
| 12 | `NO_ENTRY_FOUND` (defensive) | 409 |
| 14 | `GATE_CONFIG_MISSING` | 500 |
| 18 | `INSUFFICIENT_BALANCE` | 402 |
| 19 | `WALLET_DEBIT_FAILED` | 502 |

### Retry behaviour (no state pollution from failed EXITs)

A failed EXIT (any of the error short-circuits above) writes **zero** PVT rows and makes **zero** wallet movements. The ACTIVE entry row stays untouched.

So:
- **Failed EXIT → recharge → retry**: re-runs steps 8–21 against the same ACTIVE entry. Works.
- **Failed EXIT → retry at a different station**: fare is recomputed for the new station pair. Works.
- **Long delay between failed EXIT and retry**: overstay penalty may grow because it's computed live against `gateTimestamp`. Correct behaviour — the user is in the paid area longer.
- **Same-station retry after delay**: if `gateTimestamp - entryTime < cmrlSvpOverstaySameStationMins`, hits step 13 cancellation; else hits the regular exit flow with penalty for the duration past 20 min.

What does **not** work on retry: a passenger who walks away after a failed EXIT and later taps ENTRY at a different station (or at the same station after >20 min) will hit `NO_EXIT_FOUND` until the open ACTIVE row is closed. This is the deferred stale-entry concern.

---

## 9. Error code catalogue

Doc 2 standard envelope: `{ errorCode: Text, errorMessage: Text }`.

| `errorCode` | HTTP | Layer | Triggered by |
|---|---|---|---|
| `MISSING_HEADER` | 400 | combinator | One of the five required headers missing |
| `BAD_ALGORITHM` | 400 | combinator | `X-Signature-Alg` ≠ `"SHA256-EdDSA"` |
| `BAD_TIMESTAMP` | 400 | combinator | `X-Timestamp` doesn't parse as ISO 8601 UTC |
| `STALE_TIMESTAMP` | 401 | combinator | `\|server_now - X-Timestamp\| > skew window` |
| `UNKNOWN_KID` | 401 | combinator | `X-Signature-Kid` doesn't match any loaded CMRLConfig's `cmrlGateSigningKid` |
| `BAD_DIGEST` | 400 | combinator | Body hash doesn't match `Digest` header |
| `BAD_SIGNATURE` | 400 | combinator | Signature isn't valid base64 / Ed25519 bytes |
| `INVALID_SIGNATURE` | 401 | combinator | Signature mathematically doesn't verify |
| `INVALID_PASS` | 404 | handler | Pass not found, or pass is not an SVP (passEnum mismatch) |
| `EXPIRED_PASS` | 401 | handler | `today > endDate` |
| `INSUFFICIENT_BALANCE` | 402 | handler | Wallet balance below required (entry min or exit total) |
| `DUPLICATE_ENTRY` | 409 | handler | **ENTRY** scan at the same station as the still-open prior ENTRY, within the 20-min same-station window — passenger has just entered and is re-scanning |
| `NO_EXIT_FOUND` | 409 | handler | **ENTRY** scan, but an open ACTIVE trip already exists — at a different station OR at the same station beyond 20 min — passenger has an unclosed prior trip (visit EFO) |
| `NO_ENTRY_FOUND` | 409 | handler | **EXIT** scan, but no open ACTIVE row exists for this pass (or the row is missing its source station) — nothing to close |
| `GATE_CONFIG_MISSING` | 500 | handler | No CMRL config row exists for the pass's merchant/city |
| `WALLET_DEBIT_FAILED` | 502 | handler | Wallet debit helper raised after our pre-check passed |

Implementation: a single `CmrlGateError` ADT in `src/Tools/Error.hs` carrying each variant with a `Text` payload (typically the pass identifier for logging). The existing `IsBaseError` / `IsHTTPError` / `IsAPIError` pipeline serialises it to JSON automatically with the right HTTP status.

---

## 10. Files touched

### NammaDSL-managed (YAML edits, then `, run-generator --apply-hint`)

- `spec/Storage/PassType.yaml` — add `SVP` to `PassEnum`
- `spec/Storage/PurchasedPass.yaml` — `PassVerifyTransaction` block extensions (4 new fields, 2 enum decls, 2 queries, NOT NULL relaxations, default block)

### Hand-written (new)

- `src/Tools/EdDsaSignatureAuth.hs` — pure verifier + Servant combinator + `VerifierConfigResolver`
- `src/Domain/Action/External/CMRL/SVP.hs` — handler (Deps, pure overstay penalty, `handleEntry`, `handleExit`, `writeRow`)
- `src/API/Action/External/CMRL.hs` — Servant API type + route handler
- `src/SharedLogic/IntegratedBPPConfig/CMRL.hs` — `findCMRLConfig` extracted from `MetroBusinessHour`
- `test/Tools/EdDsaSignatureAuthSpec.hs`
- `test/Domain/Action/External/CMRL/SVPSpec.hs`

### Hand-written (modify)

- `src/Domain/Types/Extra/IntegratedBPPConfig.hs` — extend `CMRLConfig` with the 6 new fields
- `src/Tools/Error.hs` — append `CmrlGateError` ADT + instances
- `src/Storage/Queries/PassVerifyTransactionExtra.hs` — update `findLastVerifiedVehicleNumberByPurchasePassId` for the `Maybe fleetId` / `Maybe validTill` change
- `src/Domain/Action/UI/Pass.hs` — initialise the new PVT fields in the existing record literal; wrap `fleetId` / `validTill` writes in `Just`
- `src/SharedLogic/Scheduler/Jobs/MetroBusinessHour.hs` — switch from local `getCMRLConfig` to the extracted helper
- `src/API/UI.hs` — mount `ExternalCMRL.API` + `ExternalCMRL.handler` (next to `ChangeServiceTier`)
- `src/App/Server.hs` — build a `VerifierConfigResolver` and inject it into the Servant context

### Generated (auto, do not hand-edit)

- `src-read-only/Domain/Types/{PassType, PurchasedPass, PassVerifyTransaction}.hs`
- `src-read-only/Storage/Beam/PassVerifyTransaction.hs`
- `src-read-only/Storage/Queries/PassVerifyTransaction.hs`
- `src-read-only/Storage/Queries/OrphanInstances/PassVerifyTransaction.hs`
- `dev/migrations-read-only/rider-app/pass_verify_transaction.sql`

---

## 11. Task breakdown

### Task 0 — Read-only investigation (no edits)

- `src/Domain/Action/UI/SVP.hs` (EXIT branch) — note the exact wallet helper module + function names used for read + debit. Confirm they accept just `personId + amount` (or document what extra args we need).
- `src/ExternalBPP/ExternalAPI/CallAPI.hs` + `Metro/CMRL/V2/GetFare.hs` + `FareByOriginDest.hs` — exact `getFares` signature, return type, same-station behaviour.
- `src/SharedLogic/Scheduler/Jobs/MetroBusinessHour.hs:97-98` — exact `getCMRLConfig` shape for extraction.

If any of these differ from what this plan assumes, adjust the relevant downstream tasks before implementing.

### Task 1 — Schema: `SVP` PassEnum

Append `, SVP` to `enum:` in `spec/Storage/PassType.yaml`. Regenerate. Build. Commit.

### Task 2 — Schema: `PassVerifyTransaction` extensions

Edit `spec/Storage/PurchasedPass.yaml` per §5b. Regenerate. Confirm migration deltas are the expected `DROP NOT NULL` + `ADD COLUMN`s. Patch existing record literal in `Pass.hs`. Update `PassVerifyTransactionExtra.hs` for the `Maybe` field changes. Build clean. Commit.

### Task 3 — Extend `CMRLConfig`

Add the 8 `Maybe` fields per §4a to `src/Domain/Types/Extra/IntegratedBPPConfig.hs` (two for gate signing kid+key, plus the six policy fields). No `CMRLConfig` literal sites exist in code (JSON-only construction), so no further cascade. Commit.

### Task 4 — `CmrlGateError` ADT

Append to `src/Tools/Error.hs` per §9. Three instances (`IsBaseError` / `IsHTTPError` / `IsAPIError`). Build clean. Commit.

### Task 5 — Extract shared `findCMRLConfig`

Move `getCMRLConfig` from `MetroBusinessHour.hs` to a new `src/SharedLogic/IntegratedBPPConfig/CMRL.hs`. Update `MetroBusinessHour.hs` to import + call. Commit.

### Task 6 — Pure EdDSA verifier + tests (TDD)

Write the 10-branch Hspec test suite in `test/Tools/EdDsaSignatureAuthSpec.hs` first. Run, see them fail with "Module not found". Then implement `verifyRequest` in `src/Tools/EdDsaSignatureAuth.hs` per §6. All 10 tests green. Commit.

### Task 7 — Servant combinator + resolver

In the same module, add `data EdDsaSignatureAuth`, the `HasServer` instance, `VerifierConfigResolver` newtype, and the pass-through `HasOpenApi` / `SanitizedUrl` instances. Build clean. Commit.

### Task 8 — Handler + tests (TDD)

Write the ~16-branch Hspec test suite in `test/Domain/Action/External/CMRL/SVPSpec.hs` first (the pure penalty tests included). Run, see them fail. Then implement `src/Domain/Action/External/CMRL/SVP.hs` per §7 + §8 (Deps record, production wiring, handlers, pure penalty). All tests green. Commit.

### Task 9 — Servant route module

Create `src/API/Action/External/CMRL.hs` per the API type sketched in §6. Handler shape: `\gateTs req -> withFlowHandlerAPI (DSVP.postValidateSvp gateTs req)`. Build clean. Commit.

### Task 10 — Mount + inject resolver

Update `src/API/UI.hs` (add `ExternalCMRL.API` and `.handler`). Update `src/App/Server.hs` (build `buildCmrlGateResolver` and inject into Servant context). Build clean. Commit.

### Task 11 — Apply DB migrations

Apply against local Postgres (`host=localhost port=5434 user=atlas_app_user dbname=atlas_dev`). Verify with `\d` that schemas reflect the changes. The bus rows' `fleet_id` and `valid_till` remain populated (the `DROP NOT NULL` doesn't touch existing data).

### Task 12 — End-to-end smoke test

Generate an Ed25519 keypair locally with `openssl`. Update the existing CMRL `integrated_bpp_config` row's JSON via `jsonb_set` to add the kid + base64 public key + overstay defaults (one merged JSON edit). Restart the rider-app so the in-memory `kid → key` map rebuilds with the new entry. Seed an SVP `PurchasedPass` row + ensure the test person's Juspay wallet has Rs.500. Use a small Python `cryptography.hazmat.primitives.asymmetric.ed25519` helper to emit signed curl invocations.

Run the scenario matrix:

| Scenario | Expected |
|---|---|
| ENTRY at SMM with sufficient balance | 200 Success, one ACTIVE row |
| ENTRY at SMM again, within 20 min | 409 `DUPLICATE_ENTRY` |
| ENTRY at YMR (different station) while SMM trip still open | 409 `NO_EXIT_FOUND` |
| EXIT at SMM within 20 min (cancellation) | 200 Success, CANCELLED row, no wallet movement |
| ENTRY at SMM, EXIT at WMP after 15 min, fare ≤ balance | 200 Success, EXIT row with `fare_charged`, wallet debited |
| EXIT at WMP with fare > balance | 402 `INSUFFICIENT_BALANCE`, wallet untouched, ACTIVE row untouched |
| Recharge wallet, retry EXIT | 200 Success, uses same ACTIVE row, completes |
| EXIT without prior ENTRY | 409 `NO_ENTRY_FOUND` |
| Tamper body before sending | 400 `BAD_DIGEST` |
| Wrong kid | 401 `UNKNOWN_KID` |
| `X-Timestamp` 10 min stale | 401 `STALE_TIMESTAMP` |
| Wait 130 min then EXIT at different station | 200 Success, `penalty_charged ≥ 10` |
| Wallet debit deliberately fails | 502 `WALLET_DEBIT_FAILED`, no row written |

`cabal test rider-app:tests` — all suites green.

---

## 12. Open questions for CMRL (resolve before production go-live)

1. **Key exchange channel + format** — PEM, raw 32-byte base64, or hex?
2. **One key or multiple kids in Phase 1?** Our table handles many.
3. **Production hostname** — what `Host:` value will their gates send?
4. **Latency SLA** — gates expect sub-second responses. Soft timeout for `getFares` + wallet debit?
5. **Same-station fare API behaviour** — does `getFares(A, A, SVP=103)` return 0, error, or a flat charge? We short-circuit to 0.
6. **Overstay algorithm** — confirm Rs.10 per *started* hour, Rs.100 cap, 20 min / 120 min thresholds.
7. **`NO_EXIT_FOUND` / `NO_ENTRY_FOUND` policy** — strict reject vs. fail-open?
8. **Replay-retry behaviour** — will gates retry the same signed request on transient errors? Affects whether we need a recently-seen-signatures dedup window.
9. **TLS / mTLS expectations** beyond the signature scheme?
10. **Staging URL + test gate keypair?**

---

## 13. Out of scope (follow-up plans)

- **Stale-entry handling** — auto-close on conflict, scheduled sweeper, manual EFO override. User is working on the approach.
- **`POST /sync-svp`** — offline-buffered events from gates that were down when we were unreachable. Idempotent via `eventId`.
- **`GET /sync-svp-all`** consumer — daily backfill we pull from CMRL.
- **SVP purchase + recharge flow** — creates the `PurchasedPass`, mints the CDAC SQDSR + RSA-2048 QR, integrates with Juspay top-up, enforces 5-year-from-last-recharge `endDate`.
- **SVP catalog rows** — admin-side `PassCategory` "Chennai Metro" → `PassType` "Stored Value Pass" → `Pass` SKUs (`SVP_500`, `SVP_1000`, `SVP_2000`).
- **CMRL gate-key rotation admin tooling** — Doc 2 Phase 2.
- **Multi-merchant cross-check** — `gateContext` from combinator validates against `pp.merchantOperatingCityId`. Adds when we onboard other metros.
- **Decommission of `Domain.Action.UI.SVP`** — once the new flow is live.
- **`/validate-svp` write-pending → debit → confirm-pending** failure-recovery refactor — only if monitoring shows real wallet-debit/PVT-write drift in production.
- **Redis cache layer** on top of `findActiveByPurchasePassId` — only if profiling shows need.
