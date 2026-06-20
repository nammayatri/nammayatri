# Driver Popup Payment Method (Cash / Online) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Surface the customer's payment method as a derived `isPaymentOnline :: Maybe Bool` on the driver's ride-request popup payload, persisted on the BPP `SearchRequest`.

**Architecture:** Add a `paymentInstrument` column to the BPP `SearchRequest`, persist it once from the in-memory `DriverSearchBatchInput.paymentMethodInfo` inside `initiateDriverSearchBatch` (the single funnel both popup-bearing flows pass through), then derive `isPaymentOnline` in the existing popup builder which already re-reads `SearchRequest` from DB.

**Tech Stack:** Haskell, NammaDSL (YAML codegen), Beam/Sequelize KV, Cabal, `-Werror`.

**Design doc:** `docs/superpowers/specs/2026-06-18-driver-popup-payment-method-design.md`

## Global Constraints

- **NEVER edit files in `src-read-only/`** — they are generated from YAML specs via NammaDSL. Change the spec and run the generator.
- **`-Werror` is on** — unused imports/params/dodgy imports are compile errors.
- **Build command (verification cycle):** `cd Backend && cabal build dynamic-offer-driver-app`. This codebase has no per-function unit-test harness for this layer; each task's "test" is a clean compile under `-Werror`, plus behavioral verification at the end via the integration framework.
- **Generator command:** run from `Backend/` inside the nix shell: `, run-generator` (only changed specs).
- **All paths below are relative to** `Backend/app/provider-platform/dynamic-offer-driver-app/Main/` unless they start with `Backend/` or `dev/`.
- **PaymentInstrument** is `Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument`, a sum type: `Card CardType | Wallet WalletType | UPI | NetBanking | Cash | BoothOnline`. Its Beam serialization already exists (used by `Booking.paymentInstrument`).
- **Commit convention:** `dynamic-offer-driver-app/feat: <summary>`.

---

## File Structure

- `spec/Storage/SearchRequest.yaml` — declares the new persisted field (source of generated domain/beam/migration).
- `src/Storage/Queries/SearchRequestExtra.hs` — hand-written query: the targeted `updatePaymentInstrument` KV write.
- `src/SharedLogic/SearchTry.hs` — the single central persist call inside `initiateDriverSearchBatch`.
- `src/Domain/Action/UI/SearchRequestForDriver.hs` — the popup payload type + derivation of `isPaymentOnline`.

---

### Task 1: Add `paymentInstrument` to the `SearchRequest` storage spec and regenerate

**Files:**
- Modify: `spec/Storage/SearchRequest.yaml` (imports block ~line 24; fields block, after `paymentMode` at line 93)
- Generated (do not hand-edit): `src-read-only/Domain/Types/SearchRequest.hs`, `src-read-only/Storage/Beam/SearchRequest.hs`, `dev/migrations/dynamic-offer-driver-app/*.sql`

**Interfaces:**
- Produces: `Domain.Types.SearchRequest.SearchRequest { paymentInstrument :: Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument, .. }` and Beam column `Storage.Beam.SearchRequest.paymentInstrument`.

- [ ] **Step 1: Add the import alias for `PaymentInstrument`**

In `spec/Storage/SearchRequest.yaml`, in the `imports:` block, directly under the existing `PaymentMode` line (currently line 24), add:

```yaml
  PaymentInstrument: Domain.Types.Extra.MerchantPaymentMethod
```

So the two lines read:

```yaml
  PaymentMode: Domain.Types.Extra.MerchantPaymentMethod
  PaymentInstrument: Domain.Types.Extra.MerchantPaymentMethod
```

- [ ] **Step 2: Add the field**

In the same file, in `SearchRequest.fields:`, directly under the existing `paymentMode` line (currently line 93), add:

```yaml
    paymentInstrument: Maybe PaymentInstrument
```

So the two lines read:

```yaml
    paymentMode: Maybe PaymentMode
    paymentInstrument: Maybe PaymentInstrument
```

- [ ] **Step 3: Run the generator**

Run (from `Backend/`, inside the nix shell):

```bash
, run-generator
```

Expected: regenerates `src-read-only/Domain/Types/SearchRequest.hs`, `src-read-only/Storage/Beam/SearchRequest.hs`, and emits a new migration under `dev/migrations/dynamic-offer-driver-app/`.

- [ ] **Step 4: Verify the generated field and Beam column**

Run:

```bash
grep -n "paymentInstrument" app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/SearchRequest.hs app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Storage/Beam/SearchRequest.hs
```

Expected: a `paymentInstrument :: ... Maybe ... PaymentInstrument` line in the domain type and a `B.C f (... Maybe ... PaymentInstrument)` line in the Beam type.

- [ ] **Step 5: Verify the migration was generated**

Run:

```bash
grep -rln "payment_instrument" dev/migrations/dynamic-offer-driver-app/
```

Expected: at least one `.sql` file containing an `ADD COLUMN payment_instrument` (nullable, no default) on the `search_request` table. If no migration was generated, add one by hand mirroring the most recent `search_request` `ALTER TABLE ... ADD COLUMN` migration in that directory, using a nullable text column named `payment_instrument`.

- [ ] **Step 6: Build**

Run:

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

Expected: PASS (clean under `-Werror`). The new field defaults to `Nothing` everywhere `SearchRequest` is constructed via record-wildcard or generated builders, so no construction site should break.

- [ ] **Step 7: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SearchRequest.yaml \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/SearchRequest.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Storage/Beam/SearchRequest.hs \
        Backend/dev/migrations/dynamic-offer-driver-app/
git commit -m "dynamic-offer-driver-app/feat: add paymentInstrument column to SearchRequest"
```

---

### Task 2: Add the `updatePaymentInstrument` persistence query

**Files:**
- Modify: `src/Storage/Queries/SearchRequestExtra.hs` (append a new function; module already imports `Se`, `BeamSR`, `Kernel.Types.Id`, `Kernel.Utils.Common`, `Domain.Types.SearchRequest as Domain`)

**Interfaces:**
- Consumes: `Storage.Beam.SearchRequest.paymentInstrument` (from Task 1).
- Produces: `updatePaymentInstrument :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequest -> Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument -> m ()`

- [ ] **Step 1: Add the import for `PaymentInstrument`**

In `src/Storage/Queries/SearchRequestExtra.hs`, add to the import list (alphabetically near the other `Domain.Types` imports, after line 6):

```haskell
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
```

- [ ] **Step 2: Add the query function**

Append to `src/Storage/Queries/SearchRequestExtra.hs` (mirrors the existing `updateRiderId` pattern in the same file):

```haskell
updatePaymentInstrument ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id SearchRequest ->
  Maybe DMPM.PaymentInstrument ->
  m ()
updatePaymentInstrument searchRequestId paymentInstrument =
  updateOneWithKV
    [Se.Set BeamSR.paymentInstrument paymentInstrument]
    [Se.Is BeamSR.id $ Se.Eq $ getId searchRequestId]
```

- [ ] **Step 3: Build**

Run:

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

Expected: PASS. If `Se.Is`/`Se.Eq`/`getId` usage differs, copy the exact `condition` shape from `updateAutoAssign`/`updateRiderId` in the same file (they update `search_request` by id the same way).

- [ ] **Step 4: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Storage/Queries/SearchRequestExtra.hs
git commit -m "dynamic-offer-driver-app/feat: add updatePaymentInstrument query for SearchRequest"
```

---

### Task 3: Persist the instrument centrally in `initiateDriverSearchBatch`

**Files:**
- Modify: `src/SharedLogic/SearchTry.hs` (imports; body of `initiateDriverSearchBatch`, definition starts at line 132 `initiateDriverSearchBatch searchBatchInput@DriverSearchBatchInput {..} = do`)

**Interfaces:**
- Consumes: `Storage.Queries.SearchRequestExtra.updatePaymentInstrument` (Task 2); `DriverSearchBatchInput { searchReq, paymentMethodInfo, .. }` (`paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo`, and `PaymentMethodInfo { paymentInstrument :: PaymentInstrument, .. }`).
- Produces: a persisted `search_request.payment_instrument` value before any batch is sent.

- [ ] **Step 1: Add the `QSR` import**

In `src/SharedLogic/SearchTry.hs`, add to the import list (mirroring `Domain/Action/Beckn/Select.hs:50`):

```haskell
import qualified Storage.Queries.SearchRequest as QSR
```

- [ ] **Step 2: Persist the instrument at the top of `initiateDriverSearchBatch`**

`initiateDriverSearchBatch` begins (line 132):

```haskell
initiateDriverSearchBatch searchBatchInput@DriverSearchBatchInput {..} = do
  searchTry <- createNewSearchTry
```

Insert the persist call immediately after `createNewSearchTry` (before the `withTryCatch` block) so it lands in DB before `sendSearchRequestToDrivers` re-fetches the `SearchRequest`:

```haskell
initiateDriverSearchBatch searchBatchInput@DriverSearchBatchInput {..} = do
  searchTry <- createNewSearchTry
  QSR.updatePaymentInstrument searchReq.id ((.paymentInstrument) <$> paymentMethodInfo)
```

Note: `searchReq` and `paymentMethodInfo` are both brought into scope by the
`DriverSearchBatchInput {..}` record-wildcard, so no further plumbing is needed.

- [ ] **Step 3: Build**

Run:

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

Expected: PASS. If GHC reports `paymentInstrument` is ambiguous on `PaymentMethodInfo`, confirm the accessor name with `grep -n "data PaymentMethodInfo" -A4 app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Types/Extra/MerchantPaymentMethod.hs` (it is `paymentInstrument`).

- [ ] **Step 4: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/SearchTry.hs
git commit -m "dynamic-offer-driver-app/feat: persist customer paymentInstrument on SearchRequest at batch init"
```

---

### Task 4: Expose `isPaymentOnline` on the popup payload

**Files:**
- Modify: `src/Domain/Action/UI/SearchRequestForDriver.hs` (type `SearchRequestForDriverAPIEntity` at lines 51-120; builder `makeSearchRequestForDriverAPIEntity` at lines 124-205; module already imports `Domain.Types.SearchRequest as DSR`)

**Interfaces:**
- Consumes: `searchRequest.paymentInstrument :: Maybe DMPM.PaymentInstrument` (Task 1).
- Produces: `SearchRequestForDriverAPIEntity { isPaymentOnline :: Maybe Bool, .. }` on the `GET /ui/driver/nearbyRideRequest` payload.

- [ ] **Step 1: Add the `PaymentInstrument` import**

In `src/Domain/Action/UI/SearchRequestForDriver.hs`, add near the other qualified domain imports (after the existing `import qualified Domain.Types.SearchRequest as DSR` at line 28):

```haskell
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
```

- [ ] **Step 2: Add the field to the API entity**

In `data SearchRequestForDriverAPIEntity`, add the field immediately before the final field `commissionCharges :: Maybe HighPrecMoney` (the last record field, line 119). Change:

```haskell
    safetyPlusCharges :: Maybe HighPrecMoney,
    commissionCharges :: Maybe HighPrecMoney
  }
```

to:

```haskell
    safetyPlusCharges :: Maybe HighPrecMoney,
    commissionCharges :: Maybe HighPrecMoney,
    isPaymentOnline :: Maybe Bool
  }
```

(The `$(deriveJSON defaultOptions {omitNothingFields = True} ''SearchRequestForDriverAPIEntity)` below the type means `isPaymentOnline` is omitted from JSON when `Nothing`.)

- [ ] **Step 3: Set the field in the builder**

`makeSearchRequestForDriverAPIEntity` ends its record with explicit fields followed by `..` (lines ~199-203):

```haskell
          safetyPlusCharges = Just safetyCharges,
          commissionCharges = nearbyReq.commissionCharges,
          ..
        }
  where
```

`isPaymentOnline` has no matching in-scope binding, so `..` will not fill it — set it explicitly. Change to:

```haskell
          safetyPlusCharges = Just safetyCharges,
          commissionCharges = nearbyReq.commissionCharges,
          isPaymentOnline = deriveIsPaymentOnline searchRequest.paymentInstrument,
          ..
        }
  where
```

- [ ] **Step 4: Add the derivation helper**

Add `deriveIsPaymentOnline` to the `where` block of `makeSearchRequestForDriverAPIEntity` (alongside the existing `where` helpers such as `minNonZero`):

```haskell
    deriveIsPaymentOnline :: Maybe DMPM.PaymentInstrument -> Maybe Bool
    deriveIsPaymentOnline = fmap (\case DMPM.Cash -> False; _ -> True)
```

(If `LambdaCase` is not enabled in this module, GHC will error; in that case write it without LambdaCase:)

```haskell
    deriveIsPaymentOnline :: Maybe DMPM.PaymentInstrument -> Maybe Bool
    deriveIsPaymentOnline = fmap go
      where
        go DMPM.Cash = False
        go _ = True
```

- [ ] **Step 5: Build**

Run:

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

Expected: PASS under `-Werror`. A "non-exhaustive patterns" or "unused import" warning is a failure — fix before continuing.

- [ ] **Step 6: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/SearchRequestForDriver.hs
git commit -m "dynamic-offer-driver-app/feat: expose isPaymentOnline on driver ride-request popup"
```

---

### Task 5: Full build and behavioral verification

**Files:** none (verification only).

- [ ] **Step 1: Build the whole backend**

Run:

```bash
cd Backend && cabal build all
```

Expected: PASS. Confirms no other consumer of `SearchRequest`/`SearchRequestForDriverAPIEntity` broke.

- [ ] **Step 2: Verify the migration applies**

Confirm the generated migration in `dev/migrations/dynamic-offer-driver-app/` adds a nullable `payment_instrument` column to `search_request` and contains no `NOT NULL`/default. Apply it against a dev DB if available.

- [ ] **Step 3: Estimate-flow behavioral check**

Trigger a search → on_search(estimates) → select with a **Cash** payment method, then call `GET /ui/driver/nearbyRideRequest` as a matched driver and assert the popup JSON has `isPaymentOnline: false`. Repeat with an **online** method (UPI/Card) and assert `isPaymentOnline: true`. (Use the integration test framework / `run-tests` skill, or a manual end-to-end run.)

- [ ] **Step 4: Static-offer-flow behavioral check**

Run an `*OnDemandStaticOffer` direct-quote booking (search → on_search(quotes) → init → confirm) with a Cash method and assert the post-confirm popup payload has `isPaymentOnline: false`; repeat online → `true`.

- [ ] **Step 5: Scheduled-ride survival check**

Confirm a **scheduled** booking and assert the field still appears on the popup when the allocator fires the deferred batch (the scheduled job reloads `SearchRequest` from DB, so the persisted value must survive).

- [ ] **Step 6: Absent case**

Run any popup-bearing flow where no payment instrument is set and assert `isPaymentOnline` is **absent** from the JSON (not `null`/`false`), confirming `omitNothingFields` behavior.

- [ ] **Step 7: Final commit (if any verification fixups were needed)**

```bash
git add -A
git commit -m "dynamic-offer-driver-app/test: verify isPaymentOnline across estimate, static-offer, and scheduled flows"
```

---

## Self-Review

**Spec coverage:**
- "Persist instrument on SearchRequest" → Task 1 (column) + Task 3 (write). ✅
- "Write once centrally in `initiateDriverSearchBatch`" → Task 3. ✅
- "Expose derived `isPaymentOnline`" → Task 4. ✅
- "Mirror Booking.paymentInstrument serialization" → Task 1 (same type, no custom serialization). ✅
- "Cases handled: estimate, static-offer, scheduled" → Task 5 steps 3-5. ✅
- "Case not handled: RideOtp (no popup)" → no task needed; nothing is produced for that flow. ✅
- "Backward compat: nullable column, additive omitNothing field" → Task 1 step 5, Task 4 step 2. ✅

**Type consistency:** `updatePaymentInstrument :: Id SearchRequest -> Maybe DMPM.PaymentInstrument -> m ()` (Task 2) is called as `QSR.updatePaymentInstrument searchReq.id ((.paymentInstrument) <$> paymentMethodInfo)` (Task 3) — `(.paymentInstrument) <$> (paymentMethodInfo :: Maybe PaymentMethodInfo)` yields `Maybe PaymentInstrument`. ✅ `isPaymentOnline :: Maybe Bool` (Task 4 type) matches `deriveIsPaymentOnline :: Maybe PaymentInstrument -> Maybe Bool` (Task 4 helper). ✅

**Placeholder scan:** no TBD/TODO/"handle edge cases"; every code step shows concrete code. ✅
