# Driver Popup Payment Method (Cash / Online) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Surface the customer's payment method as a derived `isPaymentOnline :: Maybe Bool` on the driver's ride-request popup payload, persisted on the BPP `SearchTry`.

**Architecture:** Add a `paymentInstrument` column to the BPP `SearchTry`, set it at row-creation time inside `buildSearchTry` (called from `createNewSearchTry` within `initiateDriverSearchBatch`), then derive `isPaymentOnline` in `makeSearchRequestForDriverAPIEntity` which already receives `searchTry` as a parameter.

> **Reviewer note (2026-06-22, @khuzema786):** Moved from `SearchRequest` to `SearchTry` — the instrument can be set at INSERT time (no separate UPDATE step), and the popup builder already has `searchTry` in scope so no extra DB read is needed.

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

- `spec/Storage/SearchTry.yaml` — declares the new persisted field (source of generated domain/beam/migration).
- `src/SharedLogic/SearchTry.hs` — passes `paymentInstrument` into `buildSearchTry`; field is set at INSERT time.
- `src/Domain/Action/UI/SearchRequestForDriver.hs` — the popup payload type + derivation of `isPaymentOnline`.

---

### Task 1: Add `paymentInstrument` to the `SearchTry` storage spec and regenerate

**Files:**
- Modify: `spec/Storage/SearchTry.yaml` (imports block; fields block, after `businessEmailDomain`)
- Generated (do not hand-edit): `src-read-only/Domain/Types/SearchTry.hs`, `src-read-only/Storage/Beam/SearchTry.hs`, `src-read-only/Storage/Queries/OrphanInstances/SearchTry.hs`, `src-read-only/Storage/Queries/SearchTry.hs`, `dev/migrations-read-only/dynamic-offer-driver-app/search_try.sql`

**Interfaces:**
- Produces: `Domain.Types.SearchTry.SearchTry { paymentInstrument :: Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument, .. }` and Beam column `Storage.Beam.SearchTry.paymentInstrument`.

- [ ] **Step 1: Add the import alias for `PaymentInstrument`**

In `spec/Storage/SearchTry.yaml`, in the `imports:` block, add:

```yaml
  PaymentInstrument: Domain.Types.Extra.MerchantPaymentMethod
```

- [ ] **Step 2: Add the field**

In the same file, in `SearchTry.fields:`, after `businessEmailDomain`, add:

```yaml
    paymentInstrument: Maybe PaymentInstrument
```

- [ ] **Step 3: Run the generator**

Run (from `Backend/`, inside the nix shell):

```bash
, run-generator
```

Expected: regenerates `src-read-only/Domain/Types/SearchTry.hs`, `src-read-only/Storage/Beam/SearchTry.hs`, `src-read-only/Storage/Queries/OrphanInstances/SearchTry.hs`, `src-read-only/Storage/Queries/SearchTry.hs`, and appends a migration to `dev/migrations-read-only/dynamic-offer-driver-app/search_try.sql`.

- [ ] **Step 4: Verify the generated field and Beam column**

```bash
grep -n "paymentInstrument" \
  app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/SearchTry.hs \
  app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Storage/Beam/SearchTry.hs
```

Expected: `paymentInstrument :: ... Maybe ... PaymentInstrument` in both files.

- [ ] **Step 5: Verify the migration**

```bash
grep "payment_instrument" dev/migrations-read-only/dynamic-offer-driver-app/search_try.sql
```

Expected: `ADD COLUMN payment_instrument text ;` (nullable, no `NOT NULL`).

- [ ] **Step 6: Build**

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

Expected: PASS under `-Werror`.

- [ ] **Step 7: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/SearchTry.yaml \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/SearchTry.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Storage/Beam/SearchTry.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Storage/Queries/OrphanInstances/SearchTry.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Storage/Queries/SearchTry.hs \
        Backend/dev/migrations-read-only/dynamic-offer-driver-app/search_try.sql
git commit -m "dynamic-offer-driver-app/feat: add paymentInstrument column to SearchTry"
```

---

### Task 2: Set `paymentInstrument` at `SearchTry` creation time

**Files:**
- Modify: `src/SharedLogic/SearchTry.hs` — add `DMPM` import, extend `buildSearchTry` signature and body, update both call sites in `createNewSearchTry`.

**Interfaces:**
- Consumes: `DriverSearchBatchInput.paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo`; `PaymentMethodInfo.paymentInstrument :: DMPM.PaymentInstrument`.
- Produces: `SearchTry { paymentInstrument = Just pmi.paymentInstrument, .. }` persisted at INSERT time.

- [ ] **Step 1: Add the `DMPM` import**

```haskell
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
```

- [ ] **Step 2: Extend `buildSearchTry` signature**

Add `Maybe DMPM.PaymentInstrument ->` as the last positional parameter before `m DST.SearchTry`.

- [ ] **Step 3: Set the field in the record constructor**

Inside `buildSearchTry`, in the `pure $ DST.SearchTry { ... }` block, add:

```haskell
        paymentInstrument = mbPaymentInstrument,
```

- [ ] **Step 4: Update both call sites in `createNewSearchTry`**

Both `buildSearchTry` calls (for the initial batch and the retry batch) should pass the instrument as the final argument:

```haskell
buildSearchTry ... driverPreference ((.paymentInstrument) <$> paymentMethodInfo)
```

`paymentMethodInfo` is in scope via the `DriverSearchBatchInput {..}` wildcard.

- [ ] **Step 5: Build**

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

Expected: PASS under `-Werror`.

- [ ] **Step 6: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/SearchTry.hs
git commit -m "dynamic-offer-driver-app/feat: set paymentInstrument on SearchTry at creation time"
```

---

### Task 3: Expose `isPaymentOnline` on the popup payload

**Files:**
- Modify: `src/Domain/Action/UI/SearchRequestForDriver.hs`

**Interfaces:**
- Consumes: `searchTry.paymentInstrument :: Maybe DMPM.PaymentInstrument` (Task 1). The builder already receives `searchTry` as a parameter — no new plumbing required.
- Produces: `SearchRequestForDriverAPIEntity { isPaymentOnline :: Maybe Bool, .. }` on the `GET /ui/driver/nearbyRideRequest` payload.

- [ ] **Step 1: Add the `PaymentInstrument` import**

```haskell
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
```

- [ ] **Step 2: Add the field to the API entity**

```haskell
    commissionCharges :: Maybe HighPrecMoney,
    isPaymentOnline :: Maybe Bool
```

(`omitNothingFields = True` already applies via the existing TH splice below the type.)

- [ ] **Step 3: Set the field in the builder**

```haskell
          isPaymentOnline = deriveIsPaymentOnline searchTry.paymentInstrument,
```

- [ ] **Step 4: Add the derivation helper**

```haskell
    deriveIsPaymentOnline :: Maybe DMPM.PaymentInstrument -> Maybe Bool
    deriveIsPaymentOnline = fmap (\case DMPM.Cash -> False; _ -> True)
```

- [ ] **Step 5: Build**

```bash
cd Backend && cabal build dynamic-offer-driver-app
```

- [ ] **Step 6: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/SearchRequestForDriver.hs
git commit -m "dynamic-offer-driver-app/feat: expose isPaymentOnline on driver ride-request popup"
```

---

### Task 4: Full build and behavioral verification

**Files:** none (verification only).

- [ ] **Step 1: Build the whole backend**

```bash
cd Backend && cabal build all
```

- [ ] **Step 2: Verify the migration**

Confirm `dev/migrations-read-only/dynamic-offer-driver-app/search_try.sql` adds a nullable `payment_instrument` column to `search_try` with no `NOT NULL`/default.

- [ ] **Step 3: Estimate-flow behavioral check**

Trigger search → select with **Cash** method → assert `GET /ui/driver/nearbyRideRequest` returns `isPaymentOnline: false`. Repeat with UPI/Card → `true`.

- [ ] **Step 4: Static-offer-flow behavioral check**

`*OnDemandStaticOffer` confirm path: assert `isPaymentOnline` matches the instrument.

- [ ] **Step 5: Scheduled-ride survival check**

Confirm a scheduled booking; assert the field survives from INSERT time into the allocator-fired batch's popup.

- [ ] **Step 6: Absent case**

No instrument set → assert `isPaymentOnline` is **absent** from the JSON (not `null`), confirming `omitNothingFields`.

- [ ] **Step 7: Final commit (if any verification fixups were needed)**

```bash
git add -A
git commit -m "dynamic-offer-driver-app/test: verify isPaymentOnline across estimate, static-offer, and scheduled flows"
```

---

## Self-Review

**Spec coverage:**
- "Persist instrument on SearchTry at creation time" → Task 1 (column) + Task 2 (set in buildSearchTry). ✅
- "No separate UPDATE step" → instrument is set during INSERT via `buildSearchTry`. ✅
- "Expose derived `isPaymentOnline`" → Task 3. ✅
- "Mirror Booking.paymentInstrument serialization" → Task 1 (same type, proven Beam serialization). ✅
- "Cases handled: estimate, static-offer, scheduled" → Task 4 steps 3-5. ✅
- "Case not handled: RideOtp (no popup)" → no task needed. ✅
- "Backward compat: nullable column, additive omitNothing field" → Task 1 step 5, Task 3 step 2. ✅

**Type consistency:** `buildSearchTry ... ((.paymentInstrument) <$> paymentMethodInfo)` — `(.paymentInstrument) <$> (paymentMethodInfo :: Maybe PaymentMethodInfo)` yields `Maybe PaymentInstrument` ✅. `isPaymentOnline :: Maybe Bool` (Task 3 type) matches `deriveIsPaymentOnline :: Maybe PaymentInstrument -> Maybe Bool` (Task 3 helper). ✅

**Placeholder scan:** no TBD/TODO/"handle edge cases"; every code step shows concrete code. ✅
