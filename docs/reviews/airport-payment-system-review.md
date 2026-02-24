# Airport Flow Payment System - Implementation Plan Review

## 1. Executive Summary

The plan describes a coherent airport payment system with sensible zone separation and clear fee flows. However, after examining the codebase, there is a **fundamental impedance mismatch** between the proposed airport fees (one-off, event-driven, fixed-amount) and the existing `DriverFee` infrastructure (cycle-aggregated, subscription-coupled, ride-fare-derived). The current `DriverFee` record has 48 fields, of which ~20 are irrelevant for airport fees (`numRides`, `totalEarnings`, `specialZoneRideCount`, `planId`, `planMode`, `autopayPaymentStage`, `billNumber`, etc.). Reusing it without a wrapper or simplification layer will produce code that fights the domain model rather than expressing it. The plan also underestimates the coupling between phases and has a gap in the RideCredit reuse strategy that could conflate subscription prepaid balance with airport wallet balance.

The plan's flows are directionally correct. The key recommendations are: (a) introduce a thin `AirportFee` creation helper that hides `DriverFee`'s subscription baggage, (b) use a dedicated `AccountType` or tagged reference types rather than mixing airport balances into the existing `RideCredit` pool, and (c) explicitly address idempotency via Redis locking at the entry gate.

---

## 2. Architecture Review

### 2.1 Two-Zone Model (Outer/Inner)

**Assessment: Sound, with one structural concern.**

The existing `SpecialZone` type (`Backend/app/special-zone/src/Domain/Types/SpecialZone.hs`) already has `Airport` in its `Category` enum:

```haskell
data Category = Metro | Airport | School | Hospital | Station
```

The plan proposes two `SpecialZone` records per airport (outer + inner). This works if the inner zone is a strict geographical subset of the outer zone. However, the current `SpecialZone` has no concept of parent-child relationships between zones.

**Edge cases missed:**

- **Overlapping airports**: If two airports' outer zones overlap near a shared road, a driver could be "inside" both outer zones. The inner zone lookup must be by specific zone ID, not by category + location.
- **Zone boundary jitter**: GPS drift at the inner zone boundary could cause rapid entry/exit events. The plan should specify a debounce window (e.g., 30s cooldown after entry before allowing another entry event).
- **Driver already inside at system launch**: When airport fees go live, some drivers will already be inside the inner zone. The plan needs a migration strategy — either a grace period or a one-time "reset" where current inner-zone occupants are not charged an entry fee.

**Recommendation**: Add an optional `parentZoneId :: Maybe (Id SpecialZone)` to the zone model, or use a separate `AirportZoneConfig` table that pairs an outer zone ID with an inner zone ID. This makes the relationship explicit rather than relying on naming conventions.

### 2.2 "Check Unpaid -> Block -> Create Fee" Ordering

**Assessment: Correct order, but vulnerable to race conditions.**

The sequence is logically right: you must verify the driver has no outstanding debts before allowing new entry and creating a new fee. However:

**Race condition**: Two concurrent entry requests (e.g., driver's app triggers geofence entry while booth scanner also fires) could both pass the "no unpaid fees" check before either creates a new fee, resulting in **duplicate entry fees**.

The existing codebase uses Redis locks for similar scenarios. See `recordPayment` in `Domain/Action/Dashboard/RideBooking/Driver.hs:243`:

```haskell
Redis.whenWithLockRedis (recordPaymentLockKey reqDriverId) 30 $ do
```

**Recommendation**: The entry flow must acquire a Redis lock per driver:

```haskell
innerZoneEntryLockKey :: Id Driver -> Text
innerZoneEntryLockKey driverId = "AirportEntry:Lock:" <> driverId.getId

requestEntry driverId zoneId =
  Redis.whenWithLockRedis (innerZoneEntryLockKey driverId) 30 $ do
    validateNoUnpaidFees driverId
    createEntryFee driverId zoneId
```

### 2.3 Reusing `RideCredit` for Airport Wallet

**Assessment: Problematic. Recommend against.**

The `finance-kernel` library defines account types (`Backend/lib/finance-kernel/src-read-only/Lib/Finance/Domain/Types/Account.hs:29`):

```haskell
data AccountType = Asset | Liability | Revenue | Expense | External | RideCredit
```

Currently, `RideCredit` is used exclusively for prepaid subscription balances. The `SharedLogic/Finance/Prepaid.hs` module's `getPrepaidAccountByOwner` function filters by `accountType == RideCredit`:

```haskell
getPrepaidAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == RideCredit) accounts
```

If airport wallet top-ups also credit `RideCredit`, then:

1. **Balance queries conflate two balances**: `getPrepaidBalanceByOwner` returns a single number that mixes subscription credits and airport wallet credits. The subscription page cannot distinguish "you have 500 ride credits and 200 airport balance."
2. **Debit priority is undefined**: When a prepaid ride debit fires via `createPrepaidHold`, it draws from the combined balance. A driver who topped up for airport fees could have their balance consumed by subscription rides.
3. **Expiry logic breaks**: `handleSubscriptionExpiry` calculates expired credits based on total balance minus other active subscriptions' credits. Airport wallet credits would be incorrectly counted as subscription credits.

**Recommendation**: Add a new `AccountType` constructor (e.g., `AirportCredit`) to the `finance-kernel` `Account.yaml` spec. This requires a one-line YAML change + migration, and keeps the ledger semantically clean:

```yaml
# In Backend/lib/finance-kernel/spec/Storage/Account.yaml
enum: "Asset,Liability,Revenue,Expense,External,RideCredit,AirportCredit"
```

Then create `SharedLogic/Finance/Airport.hs` with analogous functions (`getAirportAccountByOwner`, `creditAirportBalance`, `debitAirportBalance`) that filter by `AirportCredit`. The structural code is identical to the prepaid module — DRY it via parameterization on `AccountType` if desired.

### 2.4 Premium Ride Fee Coexistence with Subscription

**Assessment: Needs more explicit separation.**

The plan says the premium ride fee is "independent of subscription" and "additive." However, the current `createDriverFee` function in `EndRide/Internal.hs:916` is deeply entangled with subscription logic:

- It checks `platformFeeChargesBy` (line 917)
- Looks up `SubscriptionConfig` (line 941)
- Determines `isPlanMandatoryForVariant` (line 942)
- Gets or assigns a default plan (line 943)
- Decides fee type based on plan payment mode (lines 1121-1124)

The airport premium ride fee must **not** go through this function. It has no plan, no subscription config, no platform fee calculation. Routing it through `createDriverFee` would require adding `if feeType == AIRPORT_PREMIUM_RIDE_FEE then skip_all_subscription_logic` branches throughout — a clear violation of the Big Picture principle.

**Recommendation**: The premium ride fee should be created by a separate function (see Section 4 for signatures) that is called from the EndRide flow alongside but independently of `createDriverFee`. The EndRide orchestrator should read as:

```haskell
-- In the endRide transaction
processEndRideFinance ...  -- existing subscription fee
whenInAirportZone booking $
  createAirportPremiumFee driverId booking  -- new, independent
```

### 2.5 FeeType Granularity

**Assessment: Correct granularity.**

`AIRPORT_ENTRY_FEE` and `AIRPORT_PREMIUM_RIDE_FEE` are semantically distinct:
- Entry fee is triggered by physical presence (entering inner zone)
- Premium ride fee is triggered by completing a ride touching the inner zone

They have different trigger points, different amounts, and potentially different payment rules. Merging them into a single `AIRPORT_FEE` would require a discriminator field elsewhere, adding complexity. Two separate constructors is right.

However, the existing `FeeType` enum contains subscription-specific types (`MANDATE_REGISTRATION`, `RECURRING_INVOICE`, etc.) mixed with one-off types (`CANCELLATION_PENALTY`, `ONE_TIME_SECURITY_DEPOSIT`). Adding two more one-off types is fine, but consider whether the enum needs documentation or grouping comments to remain readable as it grows.

---

## 3. Code Aesthetics Review

### 3.1 Big Picture Orchestrators

**Assessment: High risk of nested-case soup without explicit architectural guardrails.**

The existing `createDriverFee` function (`EndRide/Internal.hs:916-993`) is **78 lines** of deeply nested logic with 4+ levels of case matching. It is the cautionary tale the aesthetic principles warn against. The plan's inner-zone entry flow has similar complexity: resolve driver -> resolve zone -> check config -> check unpaid -> check wallet -> create fee -> maybe debit -> record event.

Without explicit guidance, the implementation will naturally follow the existing pattern and produce:

```haskell
-- What will probably happen without guardrails
requestInnerZoneEntry driverId zoneId = do
  zone <- findSpecialZoneById zoneId >>= fromMaybeM ...
  case zone.categoryCode of
    Airport -> do
      config <- getAirportFeeConfig zone.id
      case config of
        Nothing -> throwError ...
        Just cfg -> do
          unpaidFees <- findUnpaidAirportFees driverId
          case unpaidFees of
            (_:_) -> return NotAllowed ...
            [] -> do
              fee <- ...
              wallet <- ...
              -- more nesting
```

**What the plan should mandate**:

```haskell
-- GOOD: 4-line orchestrator
requestInnerZoneEntry :: Id Driver -> Id SpecialZone -> Flow EntryResult
requestInnerZoneEntry driverId zoneId =
  withAirportEntryLock driverId $
    validateEntryEligibility driverId
      >> resolveAirportFeeConfig zoneId
      >>= createEntryFee driverId
      >>= tryAutoDebitFromWallet
```

Each helper function should encapsulate one concern and handle its own error cases internally (e.g., `resolveAirportFeeConfig` throws `ConfigNotFound`, `validateEntryEligibility` throws `UnpaidFeesExist`).

### 3.2 DRY Opportunities

**Assessment: The plan misses a key shared abstraction.**

Entry fee creation and premium ride fee creation share structure:

| Aspect | Entry Fee | Premium Ride Fee |
|--------|-----------|-----------------|
| Amount source | Zone config | Zone config |
| Status | PAYMENT_PENDING | PAYMENT_PENDING |
| Driver resolution | Yes | Yes (from booking) |
| Zone resolution | Yes (from entry) | Yes (from booking location) |
| Plan/subscription | None | None |
| Auto-debit | Optional | No |

Both need: generate ID, look up amount from config, build a `DriverFee` record with ~30 fields set to defaults. Extract a shared builder:

```haskell
mkAirportFee ::
  (MonadFlow m) =>
  FeeType ->
  Id Driver ->
  Id MerchantOperatingCity ->
  HighPrecMoney ->
  Currency ->
  m DriverFee
mkAirportFee feeType driverId cityId amount currency = do
  feeId <- generateGUID
  now <- getCurrentTime
  pure $ defaultAirportDriverFee
    { id = feeId
    , driverId = driverId
    , feeType = feeType
    , status = PAYMENT_PENDING
    , platformFee = PlatformFee amount 0 0 currency
    , merchantOperatingCityId = cityId
    , createdAt = now
    , updatedAt = now
    , startTime = now
    , endTime = now  -- not cycle-based
    , payBy = addUTCTime gracePeriod now
    }
```

where `defaultAirportDriverFee` is a record with all subscription-related fields zeroed out. This avoids duplicating 30+ field assignments.

### 3.3 Returning to Orchestrator

**Assessment: Plan's flow descriptions imply 5-6 level call chains.**

The entry flow as described: `requestEntry -> resolveDriver -> resolveZone -> checkUnpaid -> createFee -> autoDebit -> recordEvent -> respond`. That's 7 steps, but if each calls the next, it becomes a 7-deep chain.

**Recommendation**: The orchestrator should call each step directly and thread the result:

```haskell
-- GOOD: Flat orchestrator, no passing-the-parcel
handleInnerZoneEntry driverId zoneId = do
  config  <- resolveAirportConfig zoneId        -- returns AirportFeeConfig
  _       <- validateNoUnpaidFees driverId      -- throws on failure
  fee     <- createAirportFee AIRPORT_ENTRY_FEE driverId config
  result  <- tryAutoDebit driverId fee          -- returns DebitResult
  _       <- recordEntryEvent driverId zoneId fee.id
  pure $ mkEntryResponse fee result
```

Max call depth: 2 (orchestrator -> helper -> DB query). Each helper returns to the orchestrator.

### 3.4 Pure vs Effects Separation

**Assessment: Good opportunities exist but the plan doesn't call them out.**

Pure functions that should be extracted:

```haskell
-- Pure: determines if a booking touches the inner zone
bookingTouchesInnerZone :: Booking -> [SpecialZone] -> Bool

-- Pure: calculates the fee amount from config
airportFeeAmount :: AirportFeeConfig -> FeeType -> HighPrecMoney

-- Pure: checks if driver has unpaid fees (given fee list)
hasUnpaidAirportFees :: [DriverFee] -> Bool
hasUnpaidAirportFees = any (\f -> f.status `elem` [PAYMENT_PENDING, PAYMENT_OVERDUE])

-- Pure: builds the API response
mkEntryResponse :: DriverFee -> DebitResult -> EntryResponse
```

The effectful functions (DB reads, Redis locks, payment API calls) should call these pure functions, not inline the logic.

### 3.5 Naming and Line Length

**Assessment: Several proposed names are borderline.**

Problematic names from the plan:

| Proposed Name | Chars | Issue |
|---|---|---|
| `AIRPORT_PREMIUM_RIDE_FEE` | 24 | Acceptable but long for pattern matches |
| `airportFeeDebitReferenceType` | 29 | Too long for a `Text` constant |
| `airportFeeTopupReferenceType` | 29 | Same |
| `postDriverV2CollectCash` + new FeeType filter | - | Existing name is already 23 chars |
| `AirportEntryEvent` | 17 | Fine |

Consider shorter alternatives:

| Current | Suggested | Chars saved |
|---|---|---|
| `AIRPORT_PREMIUM_RIDE_FEE` | `AIRPORT_RIDE_FEE` | 8 |
| `airportFeeDebitReferenceType` | `airportDebitRef` | 14 |
| `airportFeeTopupReferenceType` | `airportTopupRef` | 13 |

The `PREMIUM` qualifier adds little: all airport ride fees are premium by definition (it's an airport surcharge). `AIRPORT_RIDE_FEE` is sufficient to distinguish from `AIRPORT_ENTRY_FEE`.

A typical line using the long names:

```haskell
-- 94 chars - at the limit
fee <- mkAirportFee AIRPORT_PREMIUM_RIDE_FEE driverId cityId config.amount config.currency
```

vs:

```haskell
-- 79 chars - comfortable
fee <- mkAirportFee AIRPORT_RIDE_FEE driverId cityId config.amount currency
```

---

## 4. Risks & Gaps

### 4.1 Idempotency

**Assessment: Critical gap.**

The plan says "avoid duplicate fees" but provides no mechanism. For entry fees, duplicates could arise from:

1. **Concurrent requests** (booth scanner + geofence trigger)
2. **Retry after timeout** (client retries, server already created the fee)
3. **Re-entry within minutes** (driver exits and re-enters quickly)

**Recommendation**: Use a composite idempotency key:

```haskell
airportEntryIdempotencyKey :: Id Driver -> Id SpecialZone -> Text
airportEntryIdempotencyKey driverId zoneId =
  "AirportEntry:" <> driverId.getId <> ":" <> zoneId.getId

requestInnerZoneEntry driverId zoneId = do
  let lockKey = airportEntryIdempotencyKey driverId zoneId
  Redis.whenWithLockRedis lockKey 60 $ do  -- 60s window
    ...
```

For premium ride fees, idempotency is natural: one fee per booking. Add a unique constraint on `(driverId, feeType, referenceId)` where `referenceId` is the booking ID, or check existence before creation.

### 4.2 Revert/Refund Safety

**Assessment: Partial reversals are risky without proper state machine.**

The plan proposes extending the existing cash collection revert flow. The current `recordPayment` function marks fees as `COLLECTED_CASH` atomically for all pending fees. A revert would need to:

1. Mark the fee back to `PAYMENT_PENDING`
2. If wallet was debited, re-credit the wallet
3. If cash was partially collected (some fees paid, not all), handle the partial state

The existing refund statuses (`REFUND_PENDING`, `REFUNDED`, `REFUND_FAILED`) are designed for payment gateway refunds, not cash reversals.

**Recommendation**: For Phase 3, keep revert simple: only allow full revert of the most recent cash collection, within a configurable time window (e.g., 5 minutes). Do not support partial reversals in v1. Add a `revertedAt` timestamp and `revertedBy` to the fee record.

### 4.3 Config Complexity

**Assessment: Moderate concern.**

The plan requires configuration at multiple levels:

1. `TransporterConfig` (existing, per merchant operating city)
2. Airport fee config (new, per inner zone or per city)
3. Feature flags (enable/disable airport fees per city)
4. `SubscriptionConfig` (existing, potentially touched for display)

The codebase already has config sprawl — `TransporterConfig` alone has 100+ fields. Adding airport fee amounts there would be the wrong choice.

**Recommendation**: Create a dedicated `AirportFeeConfig` table/type:

```yaml
# spec/Storage/AirportFeeConfig.yaml
AirportFeeConfig:
  tableName: airport_fee_config
  fields:
    id: Id AirportFeeConfig
    specialZoneId: Id SpecialZone  -- the inner zone
    merchantOperatingCityId: Id MerchantOperatingCity
    entryFeeAmount: HighPrecMoney
    rideFeeAmount: HighPrecMoney
    currency: Currency
    autoDebitEnabled: Bool
    gracePeriodHours: Int
    createdAt: UTCTime
    updatedAt: UTCTime
```

This keeps airport config isolated and queryable by zone.

### 4.4 Phase Dependencies

**Assessment: Phases 1 and 2 have a circular dependency.**

The plan states:
- Phase 1: Create entry fees, "optional auto-debit from wallet"
- Phase 2: Wallet & top-up (ledger reference types, debit/credit)

But auto-debit **requires** the wallet infrastructure from Phase 2. Without Phase 2, Phase 1 fees can only be paid via:
- Payment link (if the existing payment order -> Juspay webhook flow is wired up for airport fee types)
- Cash at booth (Phase 3)

**Recommendation**: Restructure phases:

| Phase | Scope | Dependency |
|---|---|---|
| **1a** | Data model: FeeTypes, AirportFeeConfig, zone config | None |
| **1b** | Entry fee creation (no auto-debit), premium ride fee at EndRide | 1a |
| **2a** | Payment: wire existing payment order flow for airport FeeTypes | 1b |
| **2b** | Cash collection: extend `collectCashV2` for airport fee types | 1b |
| **3** | Wallet: AirportCredit account type, top-up, auto-debit at entry | 2a |
| **4** | Open market restriction in StartRide/DriverPool | 1b |
| **5** | Dashboard polish, customer-facing display | 2a, 2b |

This ensures drivers can pay (via payment link or cash) before wallet exists.

### 4.5 Missing Flows

1. **Migration for drivers already inside inner zone**: Not addressed. Need a one-time job that either (a) creates a "free" entry fee for current occupants or (b) adds a grace period config.

2. **Fee expiry / write-off**: What happens to unpaid fees after 30/60/90 days? The existing `badDebtDeclarationDate` field on `DriverFee` handles this for subscription fees. The plan should specify whether airport fees follow the same bad-debt lifecycle or have their own.

3. **Multi-city rollout**: The plan doesn't discuss how to enable airport fees city-by-city. Need a per-city feature flag (perhaps in `AirportFeeConfig` — presence of a config row for a zone implies enablement).

4. **Ride cancellation after entry**: If a driver enters the inner zone, gets assigned a ride, then the ride is cancelled — the entry fee still applies (it's for zone access, not per-ride). The premium ride fee should NOT be created. This should be explicitly stated.

5. **Existing `ServiceNames` integration**: The `DriverFee.serviceName` field uses `ServiceNames` (`YATRI_SUBSCRIPTION`, `PREPAID_SUBSCRIPTION`, `YATRI_RENTAL`, `DASHCAM_RENTAL`). Airport fees need a new `ServiceNames` constructor (e.g., `AIRPORT_FEE_SERVICE`). Without this, `collectCashV2` and other ServiceNames-filtered queries won't correctly scope to airport fees.

---

## 5. Concrete Suggestions

### 5.1 Key Orchestrator Type Signatures

```haskell
-- Module: Domain.Action.UI.Airport

-- Flow 1: Inner zone entry
requestInnerZoneEntry ::
  ( MonadFlow m, CacheFlow m r, EsqDBFlow m r ) =>
  Id Driver ->
  Id SpecialZone ->
  m EntryResult

-- Flow 3: Premium ride fee (called from EndRide)
createAirportRideFee ::
  ( MonadFlow m, CacheFlow m r, EsqDBFlow m r ) =>
  Id Driver ->
  Booking ->
  m (Maybe (Id DriverFee))

-- Flow 4: Auto-debit from airport wallet
tryAutoDebitAirportFee ::
  ( MonadFlow m, CacheFlow m r, EsqDBFlow m r ) =>
  Id Driver ->
  DriverFee ->
  m DebitResult

-- Flow 6: Top up airport wallet
topUpAirportWallet ::
  ( MonadFlow m, CacheFlow m r, EsqDBFlow m r ) =>
  Id Driver ->
  HighPrecMoney ->
  Currency ->
  m TopUpResult

-- Flow 7: Open market check (pure predicate + effectful lookup)
hasUnpaidAirportEntryFees ::
  ( MonadFlow m, CacheFlow m r, EsqDBFlow m r ) =>
  Id Driver ->
  m Bool
```

### 5.2 Shared Abstractions

**Airport fee builder** (parameterized by fee type):

```haskell
-- Module: SharedLogic.AirportFee

data AirportFeeInput = AirportFeeInput
  { feeType    :: FeeType
  , driverId   :: Id Driver
  , merchantId :: Id Merchant
  , cityId     :: Id MerchantOperatingCity
  , amount     :: HighPrecMoney
  , currency   :: Currency
  , zoneId     :: Id SpecialZone
  }

mkAirportDriverFee ::
  (MonadFlow m) =>
  AirportFeeInput -> m DriverFee
mkAirportDriverFee input = do
  feeId <- generateGUID
  now <- getCurrentTime
  let gracePeriod = 72 * 3600  -- from config in practice
  pure DriverFee
    { id = feeId
    , driverId = input.driverId
    , merchantId = input.merchantId
    , merchantOperatingCityId = input.cityId
    , feeType = input.feeType
    , status = PAYMENT_PENDING
    , platformFee = PlatformFee input.amount 0 0 input.currency
    , currency = input.currency
    , govtCharges = 0
    , startTime = now
    , endTime = now
    , payBy = addUTCTime gracePeriod now
    , createdAt = now
    , updatedAt = now
    , numRides = 0
    , totalEarnings = 0
    , specialZoneAmount = 0
    , specialZoneRideCount = 0
    , serviceName = AIRPORT_FEE_SERVICE
    -- All subscription fields: Nothing/default
    , planId = Nothing
    , planMode = Nothing
    , autopayPaymentStage = Nothing
    , billNumber = Nothing
    , feeWithoutDiscount = Nothing
    , offerId = Nothing
    , planOfferTitle = Nothing
    , schedulerTryCount = 0
    , notificationRetryCount = 0
    , overlaySent = False
    , amountPaidByCoin = Nothing
    , collectedBy = Nothing
    , collectedAt = Nothing
    , collectedAtVendorId = Nothing
    , badDebtDeclarationDate = Nothing
    , badDebtRecoveryDate = Nothing
    , vehicleNumber = Nothing
    , vehicleCategory = AUTO_CATEGORY
    , hasSibling = Nothing
    , siblingFeeId = Nothing
    , splitOfDriverFeeId = Nothing
    , stageUpdatedAt = Nothing
    , refundEntityId = Nothing
    , refundedAmount = Nothing
    , refundedAt = Nothing
    , refundedBy = Nothing
    , validDays = Nothing
    , cancellationPenaltyAmount = Nothing
    , addedToFeeId = Nothing
    }
```

Note: The 30+ Nothing/default fields here illustrate why `DriverFee` is a poor fit for airport fees. Long-term, consider either (a) a sum type for fee payloads or (b) a separate `AirportFee` table that references `DriverFee` only for payment orchestration.

**Unpaid fee checker** (reusable across entry check and open-market check):

```haskell
findUnpaidAirportFees ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id Driver -> m [DriverFee]
findUnpaidAirportFees driverId =
  QDF.findAllFeeByTypeServiceStatusAndDriver
    AIRPORT_FEE_SERVICE
    driverId
    [AIRPORT_ENTRY_FEE]
    [PAYMENT_PENDING, PAYMENT_OVERDUE]
```

This is called from both `requestInnerZoneEntry` (to block entry) and `hasUnpaidAirportEntryFees` (for open-market restriction).

### 5.3 Naming Adjustments

| Plan Name | Suggested | Rationale |
|---|---|---|
| `AIRPORT_PREMIUM_RIDE_FEE` | `AIRPORT_RIDE_FEE` | "Premium" is redundant; all airport ride fees are surcharges |
| `airportFeeDebitReferenceType` | `airportDebitRef` | 14 chars shorter; follows `prepaidRideDebitReferenceType` pattern but shorter |
| `airportFeeTopupReferenceType` | `airportTopupRef` | Same |
| `requestInnerZoneEntry` | Keep as-is | 22 chars, clear |
| `AirportEntryEvent` | Keep as-is | Clean |
| `AirportFeeConfig` | Keep as-is | Matches `SubscriptionConfig` pattern |

---

## Summary of Recommendations

| # | Category | Recommendation | Priority |
|---|---|---|---|
| 1 | Architecture | Add `AirportCredit` account type instead of reusing `RideCredit` | High |
| 2 | Architecture | Create separate `mkAirportDriverFee` — do NOT extend `createDriverFee` | High |
| 3 | Architecture | Add Redis lock for entry idempotency | High |
| 4 | Architecture | Add `AIRPORT_FEE_SERVICE` to `ServiceNames` | High |
| 5 | Aesthetics | Mandate flat orchestrator pattern (4-5 lines max) for entry flow | Medium |
| 6 | Aesthetics | Extract shared `AirportFeeInput -> DriverFee` builder | Medium |
| 7 | Aesthetics | Rename `AIRPORT_PREMIUM_RIDE_FEE` -> `AIRPORT_RIDE_FEE` | Low |
| 8 | Gaps | Restructure phases to remove 1-2 circular dependency | High |
| 9 | Gaps | Add migration strategy for drivers already inside zone | Medium |
| 10 | Gaps | Specify fee expiry / bad-debt policy for airport fees | Medium |
| 11 | Gaps | Create dedicated `AirportFeeConfig` table (not TransporterConfig) | Medium |
| 12 | Gaps | Clarify ride cancellation does not trigger premium ride fee | Low |
