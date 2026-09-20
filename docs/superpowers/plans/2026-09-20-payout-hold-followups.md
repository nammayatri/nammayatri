# Payout Hold Follow-ups (#9 / #10 / #11) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the three gaps left by the OwnerPayoutLiability hold on driver wallet payouts (PR #16805, branch `payouts-reliability`): revenue-recognition overcount (#9), double payout while a payout is in flight (#10), and wallet double-debit on webhook replay (#11).

**Architecture:** All three are driver-app (`dynamic-offer-driver-app`) only. #9 changes which ledger legs the SAP revenue job counts as "successful payouts". #10 adds an in-flight PayoutRequest guard inside the wallet lock before a new payout is initiated. #11 adds an "order already SUCCESS" guard inside the wallet lock of the webhook settlement branch. No schema, spec or generator changes; no new config.

**Tech Stack:** Haskell (GHC, `-Werror`), finance-kernel ledger, `lib/payment` PayoutRequest/PayoutOrder, Redis wallet lock, Postgres.

## Global Constraints

- Project builds with `-Werror`: any unused import/binding is a build failure.
- No code comments (global CLAUDE.md rule); the only exception is the existing Haddock/`Note` style already present in a touched file — do not add new prose comments.
- Never edit `src-read-only/`. None of these tasks needs the generator.
- The user runs builds. Each task ends with a build command for the user; do not claim "passes" without its output.
- Paths below are relative to `Backend/`. Driver app source root: `app/provider-platform/dynamic-offer-driver-app/Main/src` (abbreviated `$D`).
- Branch is `payouts-reliability`; commit on it with the repo convention `<sub-project>/<type>: <summary>` and the attribution trailer `Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>`.

## Background the implementer needs

Ledger legs involved in one driver wallet payout after this PR:

| Leg | referenceType | referenceId | When | Accounts |
|---|---|---|---|---|
| Hold | `WalletPayout` (`Wallet.walletReferencePayout`) | PayoutRequest id | initiate | OwnerLiability → OwnerPayoutLiability, `settlementStatus = PROCESSING`, `metadataV2.payoutOrderId = Nothing` |
| Settlement | `WalletPayoutSettlement` (`Wallet.walletReferencePayoutSettlement`) | PayoutOrder id | webhook SUCCESS | OwnerPayoutLiability → PlatformAsset, later `markEntriesAsClaimed` |
| Reversal of hold | `WalletPayout` (inherits), `entryType = Reversal`, `reversalOf = Just hold` | — | webhook failure | OwnerPayoutLiability → OwnerLiability |
| Legacy debit (payouts initiated before this release) | `WalletPayout` | PayoutOrder id | webhook SUCCESS | OwnerLiability → PlatformAsset, `metadataV2.payoutOrderId = Just payoutOrder.id` |

`PayoutRequest.status` moves `INITIATED` → `PROCESSING` (PG accepted, `payoutTransactionId = PayoutOrder.id`) → `CREDITED` / `AUTO_PAY_FAILED` / `CANCELLED` (synced from the PayoutOrder by `updatePayoutRequestStatusFromOrder` in `lib/payment/src/Lib/Payment/Domain/Action.hs`). A PG call that throws leaves `AUTO_PAY_FAILED`; only a process crash between create and execute leaves a stale `INITIATED`.

---

### Task 1 (#9): Count only successful payouts in revenue-recognition totals

**Files:**
- Modify: `$D/SharedLogic/Allocator/Jobs/Settlement/RideRevenueTotals.hs:325-348` (`fetchPayoutTotals`)

**Interfaces:**
- Consumes: `QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange :: Text -> Text -> UTCTime -> UTCTime -> Maybe Int -> Maybe Int -> m [LedgerEntry]` (already imported), `Wallet.walletReferencePayoutSettlement :: Text` (exported from `SharedLogic/Finance/Wallet.hs`), `LedgerDomain.LedgerEntry.metadataV2 :: Maybe LedgerEntryMetadata` with `payoutOrderId :: Maybe Text`.
- Produces: unchanged signature `fetchPayoutTotals :: Id DMOC.MerchantOperatingCity -> UTCTime -> UTCTime -> m (PayoutTotals, [RevenueRecognitionTransactionRow])`; `SAPRideRevenueDispatch.hs:203` keeps consuming `totals.payout` unchanged.

Why: `findSettledByReferenceTypeAndDateRange Wallet.walletReferencePayout` now returns the hold leg for every payout (in-flight and failed included; it excludes only reversals via `reversalOf = Nothing`). The success-only legs are `WalletPayoutSettlement` (new model) and, during the transition, legacy `WalletPayout` debits, which are the only `WalletPayout` legs carrying `metadataV2.payoutOrderId`.

- [ ] **Step 1: Replace the body of `fetchPayoutTotals`**

Current code (`RideRevenueTotals.hs:337-348`):

```haskell
fetchPayoutTotals merchantOpCityId fromTime toTime = do
  -- List SETTLED WalletPayout ledger legs (driver liability debit on success).
  -- Single-leg createWalletEntryDelta — no account join needed.
  rawRows <- QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange Wallet.walletReferencePayout merchantOpCityId.getId fromTime toTime Nothing Nothing
  let (totals, txnRowsRev) = foldl' go (PayoutTotals 0 0, []) rawRows
  pure (totals, reverse txnRowsRev)
  where
    go (acc, rs) le =
      ( acc {payoutAmount = acc.payoutAmount + le.amount, txnCount = acc.txnCount + 1},
        RevenueRecognitionTransactionRow {amount = le.amount, referenceId = le.referenceId, txnStatus = show le.status} : rs
      )
```

New code:

```haskell
fetchPayoutTotals merchantOpCityId fromTime toTime = do
  -- SETTLED WalletPayoutSettlement legs (OwnerPayoutLiability → PlatformAsset on webhook SUCCESS) plus
  -- legacy WalletPayout debits (payouts initiated before the OwnerPayoutLiability hold; only those carry
  -- metadataV2.payoutOrderId). The hold leg itself shares the WalletPayout reference but is posted at
  -- initiate for every payout, so it must not be counted.
  settlementRows <- QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange Wallet.walletReferencePayoutSettlement merchantOpCityId.getId fromTime toTime Nothing Nothing
  legacyRows <- filter isLegacyPayoutDebit <$> QLedgerEntryExtra.findSettledByReferenceTypeAndDateRange Wallet.walletReferencePayout merchantOpCityId.getId fromTime toTime Nothing Nothing
  let (totals, txnRowsRev) = foldl' go (PayoutTotals 0 0, []) (settlementRows <> legacyRows)
  pure (totals, reverse txnRowsRev)
  where
    isLegacyPayoutDebit le = isJust (le.metadataV2 >>= (.payoutOrderId))
    go (acc, rs) le =
      ( acc {payoutAmount = acc.payoutAmount + le.amount, txnCount = acc.txnCount + 1},
        RevenueRecognitionTransactionRow {amount = le.amount, referenceId = le.referenceId, txnStatus = show le.status} : rs
      )
```

The `-- ` lines above replace the two existing comment lines in that function (this file already documents each fetcher this way; keep it to those four lines). Also update the fetcher's Haddock two lines above (`-- | Successful wallet payouts (Juspay webhook → WalletPayout ledger). Feeds both`) to read `-- | Successful wallet payouts (webhook SUCCESS → WalletPayoutSettlement, or legacy WalletPayout debit). Feeds both`.

- [ ] **Step 2: Build**

Run (user): `cd Backend && cabal build dynamic-offer-driver-app`
Expected: no warnings/errors. If `isJust` is not in scope, it comes from `Kernel.Prelude` (already imported) — do not add `Data.Maybe`.

- [ ] **Step 3: Verify against the ledger**

On a dev DB with at least one successful, one in-flight and one failed wallet payout, run:

```sql
select reference_type, entry_type, settlement_status, (metadata_v2->>'payoutOrderId') is not null as legacy, count(*), sum(amount)
from atlas_driver_offer_bpp.finance_ledger_entry
where reference_type in ('WalletPayout','WalletPayoutSettlement') and status = 'SETTLED' and reversal_of is null
group by 1,2,3,4;
```

Expected: the sum reported by the `PayoutToClearing` JV for the window equals `sum(WalletPayoutSettlement)` + `sum(WalletPayout where legacy = true)`, and excludes the `WalletPayout` rows with `legacy = false` (holds). Trigger the job via the existing `ScheduledTDSDistribution`/SAP dispatch path used in dev, or call `computeRideRevenueTotals` from `cabal repl` and compare `payout`.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/Settlement/RideRevenueTotals.hs
git commit -m "driver/fix: count only settled payout legs in revenue recognition totals

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 2 (#11): Skip webhook settlement for an order that is already SUCCESS

**Files:**
- Modify: `$D/Domain/Action/UI/Payout.hs:470-480` (`settlePayoutEntities`, `DRIVER_WALLET_TRANSACTION` branch)

**Interfaces:**
- Consumes: `QPayoutOrder.findByOrderId :: Text -> m (Maybe PayoutOrder)` (already used in this file), `isPayoutOrderSuccess` (already imported from `Lib.Payment.Payout.StatusCheck`), `makeWalletRunningBalanceLockKey`.
- Produces: no signature change.

Why: the Juspay webhook can be delivered more than once. Today the branch runs `callPayoutServiceAction` (which syncs the order to SUCCESS) and then debits the wallet; on a replay the hold is already claimed, so `findOwnerPayoutLiabilityEntry` returns `Nothing` and the legacy branch debits the wallet a second time. `refreshPayoutOrderWithSettlement` and the status-check job already skip SUCCESS orders; the webhook must do the same, and it must re-read the order *inside* the wallet lock so two concurrent deliveries serialize. Only SUCCESS is treated as final: a failure followed later by a SUCCESS (Juspay fulfilment retry) must still settle — that case is handled correctly by the existing `Nothing` branch (the reversed hold re-credited the wallet, the legacy debit now takes it back).

- [ ] **Step 1: Re-read the order inside the lock and guard**

Current code (`Payout.hs:476-478`):

```haskell
      Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
        (updPayoutStatus, _) <- callPayoutServiceAction payoutOrder.orderId driverId payoutConfig
        person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
```

New code — wrap the whole existing lock body in an `unlessM`-style guard, re-reading the order inside the lock:

```haskell
      Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
        alreadySettled <- maybe False (isPayoutOrderSuccess . (.status)) <$> QPayoutOrder.findByOrderId payoutOrder.orderId
        if alreadySettled
          then logInfo $ "Payout order " <> payoutOrder.orderId <> " already SUCCESS, skipping wallet settlement"
          else do
            (updPayoutStatus, _) <- callPayoutServiceAction payoutOrder.orderId driverId payoutConfig
            person <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            ... -- the rest of the existing lock body, unchanged, indented four spaces deeper,
            ... -- down to and including the `Notify.sendNotificationToDriver …` line
```

`payoutConfig` is bound in the enclosing `do` (`payoutConfig <- getPayoutConfigForCustomer …`), which is why the body stays inline rather than moving to the `where` block.

- [ ] **Step 2: Build**

Run (user): `cd Backend && cabal build dynamic-offer-driver-app`
Expected: clean. If ormolu/hlint complains about the `if … then … else do` shape, `bool`/`unless` are not suitable here (the body is monadic and long); keep the `if`.

- [ ] **Step 3: Verify with a replayed webhook**

1. Initiate a wallet payout in dev; note `payout_request.id`, `payout_order.order_id`, and the driver's wallet balance.
2. Deliver the Juspay SUCCESS webhook once (`/ui/payout/juspay/webhook/...` with the dev basic-auth) → balance unchanged from post-hold (hold already moved the money), one `WalletPayoutSettlement` row, hold `settlement_status = 'PAID_OUT'`.
3. Deliver the identical webhook again.

Expected after step 3: log line `already SUCCESS, skipping wallet settlement`; no new `finance_ledger_entry` rows for the driver; balance unchanged; exactly one FCM `PAYOUT_COMPLETED`.

```sql
select reference_type, count(*) from atlas_driver_offer_bpp.finance_ledger_entry
where reference_id in ('<payout_request_id>','<payout_order_id>') group by 1;
```

Expected: `WalletPayout` 1, `WalletPayoutSettlement` 1 (plus `PGPayoutCharges` legs if fee bearer is configured) — same counts before and after the replay.

- [ ] **Step 4: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Payout.hs
git commit -m "driver/fix: skip wallet payout settlement when order already SUCCESS

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

### Task 3 (#10): Refuse a new wallet payout while one is in flight

**Files:**
- Modify: `$D/SharedLogic/PayoutStatusCheck.hs:35-52` (split `getPayoutStatusCheckConfig` so the horizon can be resolved by person)
- Modify: `$D/SharedLogic/Finance/WalletPayout.hs:52-58` (`WalletPayoutParams`), `:133-150` (`walletPayoutStep`)
- Modify: `$D/Domain/Action/UI/DriverWallet.hs:575-581` (`postWalletPayout` params)
- Modify: `$D/SharedLogic/Allocator/Jobs/Payout/ScheduledBatchPayout.hs:261-266` (`processOneWalletPayout` params)

**Interfaces:**
- Consumes: `QPayoutRequestExtra.findByBeneficiaryWithFilters :: Text -> Maybe UTCTime -> Maybe UTCTime -> [PayoutRequestStatus] -> Maybe Int -> Maybe Int -> m [PayoutRequest]` (filters on `createdAt`), `PR.INITIATED`, `PR.PROCESSING`, `PSC.PayoutStatusCheckConfig {checkInterval :: NominalDiffTime, maxAttempts :: Int}`.
- Produces:
  - `SharedLogic.PayoutStatusCheck.getPayoutStatusCheckConfigForPerson :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m PSC.PayoutStatusCheckConfig`
  - `WalletPayoutParams.throwOnPayoutInFlight :: Bool` (new field)

Why: after the hold, a second payout for the same driver sees the reduced balance, so the normal path is safe. Two windows remain: (a) payouts initiated by the pre-hold code that are still `PROCESSING` at rollout — no hold exists, the wallet still shows the full balance; (b) `postOwnerPayoutLiability` failing after the PG accepted the order (`WalletPayout.hs:216`, only logged). In both, a `PayoutRequest` for the driver is `INITIATED`/`PROCESSING`. Refusing while such a request exists closes both. The lookback is bounded by the status-check horizon (`checkInterval × maxAttempts`, 48h by default) so a request abandoned by the checker cannot block the driver forever; a `PROCESSING` request older than that is exactly the case the `payout_status_check_max_attempts_reached` metric flags for ops.

- [ ] **Step 1: Expose the status-check horizon by person**

In `$D/SharedLogic/PayoutStatusCheck.hs`, replace `getPayoutStatusCheckConfig` with:

```haskell
getPayoutStatusCheckConfig :: (CacheFlow m r, EsqDBFlow m r) => DPayoutOrder.PayoutOrder -> m PSC.PayoutStatusCheckConfig
getPayoutStatusCheckConfig order = do
  merchantOperatingCityId <- case order.merchantOperatingCityId of
    Just mocId -> pure (Id mocId)
    Nothing -> do
      person <- QP.findById (Id order.customerId) >>= fromMaybeM (PersonNotFound order.customerId)
      pure person.merchantOperatingCityId
  getPayoutStatusCheckConfigForPerson (Id order.customerId) merchantOperatingCityId

getPayoutStatusCheckConfigForPerson :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m PSC.PayoutStatusCheckConfig
getPayoutStatusCheckConfigForPerson personId merchantOperatingCityId = do
  mbVehicle <- QV.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <-
    getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Nothing}) Nothing
      >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOperatingCityId.getId)
  pure
    PSC.PayoutStatusCheckConfig
      { checkInterval = secondsToNominalDiffTime (fromIntegral payoutConfig.payoutStatusCheckInterval),
        maxAttempts = payoutConfig.payoutStatusCheckMaxAttempts
      }
```

Add to the module export list `getPayoutStatusCheckConfigForPerson,` and add imports:

```haskell
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
```

`QV.findById` takes `Id Person` in this app (see the existing call `QV.findById (Id order.customerId)`), so passing `personId` directly is type-correct.

- [ ] **Step 2: Add the guard to `walletPayoutStep`**

In `$D/SharedLogic/Finance/WalletPayout.hs`:

Extend the params record:

```haskell
data WalletPayoutParams = WalletPayoutParams
  { payoutType :: PR.PayoutType,
    minimumPayoutAmount :: HighPrecMoney,
    enforceDailyLimit :: Bool,
    throwOnBelowMinimum :: Bool,
    throwOnPayoutInFlight :: Bool
  }
```

Add a helper below `resolvePayoutVpa`:

```haskell
findPayoutInFlight :: (CacheFlow m r, EsqDBFlow m r, BeamFlow m r) => PayoutContext -> UTCTime -> m (Maybe PR.PayoutRequest)
findPayoutInFlight ctx now = do
  config <- getPayoutStatusCheckConfigForPerson ctx.driverId ctx.mocId
  let lookback = config.checkInterval * fromIntegral config.maxAttempts
  listToMaybe <$> QPayoutRequestExtra.findByBeneficiaryWithFilters ctx.driverId.getId (Just (addUTCTime (negate lookback) now)) Nothing [PR.INITIATED, PR.PROCESSING] (Just 1) Nothing
```

Change the top of `walletPayoutStep` from:

```haskell
walletPayoutStep ctx params = do
  now <- getCurrentTime
  let counterparty = counterpartyFromRole ctx.person.role
```

to:

```haskell
walletPayoutStep ctx params = do
  now <- getCurrentTime
  mbInFlight <- findPayoutInFlight ctx now
  case mbInFlight of
    Just inFlight
      | params.throwOnPayoutInFlight -> throwError $ InvalidRequest ("Payout already in progress: " <> inFlight.id.getId)
      | otherwise -> logInfo $ "Skipping wallet payout for " <> ctx.driverId.getId <> ": payoutRequest " <> inFlight.id.getId <> " is " <> show inFlight.status
    Nothing -> walletPayoutEligibleStep ctx params now

walletPayoutEligibleStep :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> UTCTime -> m ()
walletPayoutEligibleStep ctx params now = do
  let counterparty = counterpartyFromRole ctx.person.role
```

(the rest of the old `walletPayoutStep` body, unchanged, now lives in `walletPayoutEligibleStep`; drop its own `now <- getCurrentTime` since `now` is a parameter).

Add imports:

```haskell
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated, getPayoutStatusCheckConfigForPerson)
```

(the second replaces the existing `import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)`). `addUTCTime` and `listToMaybe` come from `Kernel.Prelude`.

- [ ] **Step 3: Set the new flag at both call sites**

`$D/Domain/Action/UI/DriverWallet.hs` (`postWalletPayout`):

```haskell
        WalletPayoutParams
          { payoutType = PR.INSTANT,
            minimumPayoutAmount = ctx.transporterConfig.driverWalletConfig.minimumWalletPayoutAmount,
            enforceDailyLimit = True,
            throwOnBelowMinimum = True,
            throwOnPayoutInFlight = True
          }
```

`$D/SharedLogic/Allocator/Jobs/Payout/ScheduledBatchPayout.hs` (`processOneWalletPayout`):

```haskell
            WalletPayoutParams
              { payoutType = PR.SCHEDULED,
                minimumPayoutAmount = config.minimumPayoutAmount,
                enforceDailyLimit = False,
                throwOnBelowMinimum = False,
                throwOnPayoutInFlight = False
              }
```

- [ ] **Step 4: Build**

Run (user): `cd Backend && cabal build dynamic-offer-driver-app`
Expected: clean. Likely `-Werror` trips: unused `now` binding if it was not removed from `walletPayoutEligibleStep`; missing `BeamFlow m r` on `findPayoutInFlight` (it is part of `WalletPayoutFlow`, so the helper may simply use `(WalletPayoutFlow m r) =>` instead of the narrower constraint).

- [ ] **Step 5: Verify the three cases**

Case A — normal double tap: initiate a payout, then call `POST /ui/wallet/payout` again before the webhook.
Expected: HTTP 400 `Payout already in progress: <payoutRequestId>`; no second `payout_request` row; wallet balance unchanged.

Case B — legacy in-flight (simulates the deploy window): with the wallet balance ≥ minimum, insert a `PROCESSING` `payout_request` for the driver with `created_at = now()` and no hold leg (do not create a ledger row), then call `POST /ui/wallet/payout`.
Expected: same 400; nothing posted to the ledger.

Case C — stale request outside the horizon: update that row's `created_at` to `now() - interval '3 days'` (default horizon is 8 × 21600s = 48h) and call again.
Expected: payout proceeds normally (new `payout_request`, hold leg posted).

Case D — scheduled batch: with a `PROCESSING` request in the window, run the `ScheduledBatchPayout` job for the city.
Expected: log `Skipping wallet payout for <driverId>: payoutRequest <id> is PROCESSING`, no error log, job completes.

```sql
select id, status, created_at from atlas_driver_offer_bpp.payout_request
where beneficiary_id = '<driverId>' order by created_at desc limit 5;
```

- [ ] **Step 6: Commit**

```bash
git add Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/PayoutStatusCheck.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Finance/WalletPayout.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverWallet.hs \
        Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/Payout/ScheduledBatchPayout.hs
git commit -m "driver/fix: refuse wallet payout while a payout request is in flight

Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"
```

---

## Rollout notes

- Order of tasks does not matter for correctness; Task 2 and Task 3 are the ones to have in place *before* the release that introduces the hold, because the deploy window is exactly when they matter.
- Task 3's horizon reuses `payout_config.payout_status_check_interval × payout_status_check_max_attempts`; cities with no `payout_config` row for the driver's vehicle category will get `PayoutConfigNotFound` from `postWalletPayout` — that is the same precondition `afterPayoutOrderCreated` already has, so no new configuration is required.
- No migration, no spec change, no generator run.
