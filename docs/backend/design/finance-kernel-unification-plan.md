# Finance-Kernel Unification — Transfer, Config, and Status Ownership

Status: PROPOSED 2026-10-23 (audit-backed; supersedes the sketch discussed on 2026-10-23)
Owner: finance platform
Scope: `Backend/lib/finance-kernel`, with adoption changes in driver-app, rider-app, `lib/payment`

---

## 0. The problem, grounded in the audit

The kernel today is a **ledger** (accounts, double-entry, invoices, tax, recon, settlement
ingestion) with exactly one boundary helper (`Ledger/PayoutSettlement.hs`, 90 lines). Everything
that touches the *outside world* — payouts, topups, refund rails, SDK payloads — is re-implemented
per flow in the apps:

| Re-implementation | Where | LOC (approx) |
|---|---|---|
| Driver wallet payout initiate (2 rail orderings + compensation ladder) | `SharedLogic/Finance/WalletPayout.hs` | 446 |
| Driver payout settlement dispatch (10 entityName branches) | `Domain/Action/UI/Payout.hs` (BPP) | ~400 |
| Driver wallet topup (order → webhook → credit) | `DriverWallet.hs` + `Payment.hs` webhook | ~150 |
| Prepaid credit/debit (bypasses FinanceM entirely, 9 raw `createEntryWithBalanceUpdate`) | `SharedLogic/Finance/Prepaid.hs` | 1321 |
| Rider cashback payout (reserve/submit/outcome branches) | `SharedLogic/Finance/CashbackPayout.hs` + `UI/Payout.hs` | ~370 |
| Booking-deposit hold/credit/refund + hand-computed available balance | `SharedLogic/BookingDeposit*.hs` | ~870 |
| Rail-status → domain-status mapping tables | ≥6 copies across both apps + `lib/payment` | — |
| Unledgered payouts (referral, backlog, metro cashback) and unledgered refunds (FRFS, tickets, parking) | rider-app | — |

Money status lives in **six+ enums across four packages** (`PayoutOrderStatus` — 24 ctors,
`TransactionStatus` — 13, `PayoutRequestStatus` — 10, `PayoutStatus`, `PayoutBatchStatus`,
`WalletTransactionStatus`, `ScheduledPayoutStatus`, `CashbackStatus`, …), with the rail→domain
mapping copied per caller via `refreshPayoutStatus`'s four injected callbacks.

Config is scattered: `DriverWalletConfig` (30 fields, 2 dead), `ScheduledPayoutConfig`
(city × payoutCategory — right shape, wrong key), `PayoutConfig` (driver + rider variants),
`RiderConfig` refund fields. The kernel's own config hook (`Core/Types.FinanceEnv`/`ConfigKey`/
`ConfigValue`) is dead scaffolding with zero references. `FinanceError.InsufficientBalance`
exists and is never raised — the kernel will debit any account to any negative balance.

**Dependency constraint (hard):** `lib/payment` depends on `lib/finance-kernel`
(`lib/payment/package.yaml:90`). The kernel can never import `Lib.Payment.*`. Everything
rail-shaped must be **injected** — the house patterns are `RunFinance`
(`PayoutSettlement.hs:31`) and the `Recipe m` record-of-functions + app-side registry
(`Reconciliation/{Recipe,Job}.hs`).

---

## 1. Design principles (= the rules, refined)

1. **All money-movement config keys on the account class**, not the domain:
   `(merchantOperatingCityId, accountType, counterpartyType, subLedger)` with a wildcard ladder.
   The kernel never learns "wallet" — a driver wallet is just `(Liability, DRIVER)`; a prepaid
   balance is `(RideCredit, DRIVER, subLedger=PREPAID)`; the rider deposit wallet is
   `(Liability, RIDER)`.
2. **The kernel handles accounts generically** — no domain vocabulary (no driver, ride,
   booking, VPA). Domains map their concepts onto account classes and reference types.
3. **All *money* statuses live in the kernel** — one canonical `TransferStatus`, with the raw
   rail status carried as an opaque passthrough. Domains may keep presentation enums but must
   derive them from the kernel status, never maintain a parallel lifecycle.
4. **Typed metadata for flow artifacts only** (rail order id, UTR, fee breakdown, covered
   ledger refs). Domain information never goes in metadata — the counterparty fields already
   identify the owner.
5. **Rails are injected** (record-of-functions, app-side registry). The kernel owns the
   orchestration, ordering, compensation, idempotency, and status; the rail record owns
   "how to talk to Juspay/Stripe/HDFC" and the one rail→canonical status mapping.

### Corrections to the original sketch (things that looked right but aren't)

- **"transfer = pending entries + call payout" as one atomic verb is wrong**: the rail
  ordering differs per rail. HDFC bulk must hold *before* submit (file is sent later, full
  compensation ladder exists today); Juspay/Stripe submit *first* and hold after (today a
  failed hold is merely logged — a real bug this design fixes). So reserve and submit are
  separable steps inside one kernel orchestration, with the ordering declared by the rail.
- **`AllMoney | PartialMoney` must resolve inside the kernel, under the balance lock** —
  "all" means `redeemable − live holds − minRetained − fee`, which is config-dependent and
  racy if computed by the domain first.
- **The config key needs `subLedger` and a wildcard ladder**, otherwise prepaid vs wallet
  can't diverge and every city needs a full row set.
- **Negative balance is two knobs, not one**: `allowNegativeBalance` is a *posting*
  permission (may this account go below zero at all — cash commission needs yes);
  the eligibility floor on *available* balance is a separate generic knob
  (`minRetainedBalance`) that domains read, not a posting rule.
- **Do not lift all 10 `PayoutRequestStatus` states into the kernel.** `CASH_PAID`,
  `CASH_PENDING`, `EXCLUDED` are domain decisions about *not* using a rail; the kernel gets
  7 canonical states + opaque `railStatus`.

---

## 2. New module map

```
lib/finance-kernel/
  spec/Storage/
    FinanceAccountConfig.yaml        -- NEW table
    Transfer.yaml                    -- NEW table (finance_transfer)
    ScheduledTransferRule.yaml       -- NEW table (replaces ScheduledPayoutConfig shape)
  src/Lib/Finance/
    AccountConfig.hs                 -- NEW: resolution ladder + typed config
    Balance.hs                       -- NEW: BalanceBreakdown, resolveTransferAmount, lock
    Transfer/
      Types.hs                       -- NEW: Transfer, TransferStatus, TransferOutcome, Rail types
      Rail.hs                        -- NEW: TransferRail record + RailRegistry
      Internal.hs                    -- NEW: FA→FA instant transfers
      Outbound.hs                    -- NEW: FA→real (payout) orchestration
      Inbound.hs                     -- NEW: real→FA (topup/recharge) orchestration
      Outcome.hs                     -- NEW: applyTransferOutcome / applyRailStatus / statusCheck
      Scheduled.hs                   -- NEW: rules, next-run computation, sweep driver
      History.hs                     -- NEW: unified account history + settled-ledger reads
  (absorbs/retires)
    Ledger/PayoutSettlement.hs       -- becomes internal to Transfer/Outcome.hs
    Core/Types.FinanceEnv/ConfigKey/ConfigValue  -- deleted (dead)
```

---

## 3. `finance_account_config` — the central config

### 3.1 Table (NammaDSL storage spec)

```yaml
FinanceAccountConfig:
  fields:
    id: Id FinanceAccountConfig
    merchantId: Text
    merchantOperatingCityId: Text        # '*' allowed (merchant-wide default)
    accountType: AccountType             # existing kernel enum
    counterpartyType: Maybe CounterpartyType   # Nothing = wildcard
    subLedger: Maybe Text                # Nothing = wildcard
    # ---- posting rules ----
    allowNegativeBalance: Bool           # posting permission; enforced in createEntryWithBalanceUpdate
    # ---- balance semantics ----
    minRetainedBalance: HighPrecMoney    # subtracted when resolving AllAvailable; default 0
    redeemableCutoff: Maybe CutoffSpec   # JSON: {days :: Int, timeOfDay :: Text, tzOffsetSeconds :: Int}
    # ---- outbound transfer (payout) ----
    transferOutEnabled: Bool
    instantTransferOutEnabled: Bool
    minTransferOutAmount: HighPrecMoney
    maxTransferOutPerDay: Maybe Int
    transferOutFee: Maybe FeeSpec        # JSON: {bearer :: OWNER|PLATFORM, fixedFee, percentageRate}
    # ---- inbound transfer (recharge/topup) ----
    transferInEnabled: Bool
    # ---- scheduling hook ----
    autoTransferOut: Maybe Text          # name of a ScheduledTransferRule; Nothing = manual only
  constraints:
    id: PrimaryKey
```

Lookups go through a cached query keyed on the full class tuple (note: `merchantOperatingCityId`
and `status` are on the KV deny list, so this is a CachedQuery over a plain PK table — same
pattern as other config tables). The config is registered in config-pilot as
`SFinanceAccountConfig` in the `SConfigType` GADT so Studio/rulebooks can patch it — **no
bespoke read path**.

### 3.2 Resolution ladder (most-specific wins, whole-row)

```haskell
-- Lib.Finance.AccountConfig
data AccountClass = AccountClass
  { merchantId              :: Text
  , merchantOperatingCityId :: Text
  , accountType             :: AccountType
  , counterpartyType        :: Maybe CounterpartyType
  , subLedger               :: Maybe Text
  }

-- Ladder, first hit wins:
--   1. (city, acctType, cpType, subLedger)
--   2. (city, acctType, cpType, *)
--   3. (city, acctType, *, *)
--   4. ('*', acctType, cpType, subLedger)
--   5. ('*', acctType, cpType, *)
--   6. ('*', acctType, *, *)
--   7. compiled-in defaults (defaultAccountConfig)
getAccountConfig  :: (BeamFlow m r, CacheFlow m r) => AccountClass -> m ResolvedAccountConfig
classOfAccount    :: Account -> AccountClass

data ResolvedAccountConfig = ResolvedAccountConfig
  { allowNegativeBalance     :: Bool
  , minRetainedBalance       :: HighPrecMoney
  , redeemableCutoff         :: Maybe CutoffSpec
  , transferOutEnabled       :: Bool
  , instantTransferOutEnabled :: Bool
  , minTransferOutAmount     :: HighPrecMoney
  , maxTransferOutPerDay     :: Maybe Int
  , transferOutFee           :: Maybe FeeSpec
  , transferInEnabled        :: Bool
  , autoTransferOut          :: Maybe Text
  , resolvedFrom             :: AccountClass   -- which row won; for debugging/ops
  }

defaultAccountConfig :: ResolvedAccountConfig   -- permissive-read, restrictive-move:
-- allowNegativeBalance = True (today's behaviour — flip per class, not globally)
-- transferOutEnabled = False, transferInEnabled = False, everything else zero/Nothing
```

### 3.3 What maps where (driver examples)

| Today | Becomes |
|---|---|
| `DriverWalletConfig.enableWalletPayout` | `transferOutEnabled` on `(Liability, DRIVER)` |
| `minimumWalletPayoutAmount` | `minTransferOutAmount` on `(Liability, DRIVER)` |
| `maxWalletPayoutsPerDay` | `maxTransferOutPerDay` |
| `payoutCutOffDays` (+ hardcoded cutoff time) | `redeemableCutoff` |
| `payoutFee :: PayoutFeeConfig` | `transferOutFee` |
| `instantPayoutExcludedRoles = [FLEET_BUSINESS]` | `instantTransferOutEnabled = False` on `(Liability, FLEET_OWNER)` row |
| `enableWalletTopup` (dead) | `transferInEnabled` on `(Liability, DRIVER)` — revived with meaning |
| `minWalletAmountForCashRides` | **stays domain** (ride-eligibility policy), but reads kernel `available` which already nets `minRetainedBalance` |
| cash-commission may drive wallet negative | `allowNegativeBalance = True` on `(Liability, DRIVER)` — now *declared*, not accidental |
| rider deposit spendable-balance hand-math | `getBalanceBreakdown` on `(Liability, RIDER)` |
| `driverWalletPayoutThreshold`, `gstPercentage` (dead) | deleted |

Fields that stay in domain configs because they are domain policy, not account policy:
`clubProjectFareIn*`, `fleetLinkedDriverWalletView`, `forceOnlineLedger`,
`onlineCommissionPaidOutDirectly`, `enableWalletGatedTierCheck`, connect-account charge fields,
`fetchWalletTransactionsFromClickhouse`, referral thresholds, `enablePaymentRefunds`.

---

## 4. Balance — one source of truth

```haskell
-- Lib.Finance.Balance
data BalanceBreakdown = BalanceBreakdown
  { ledgerBalance   :: HighPrecMoney   -- account.balance
  , pendingHolds    :: HighPrecMoney   -- PENDING ledger holds (statutory, deposit, prepaid)
  , offerHolds      :: HighPrecMoney   -- Redis TempBalanceHold total (key derived from account)
  , processingOut   :: HighPrecMoney   -- PayoutLiability sibling balance (in-flight transfers)
  , nonRedeemable   :: HighPrecMoney   -- post-cutoff unsettled net (existing getPayoutEligibilityData math)
  , redeemable      :: HighPrecMoney   -- ledgerBalance − nonRedeemable (floor 0)
  , available       :: HighPrecMoney   -- redeemable − pendingHolds − offerHolds − minRetained (floor 0)
  , redeemableEntryIds :: [Id LedgerEntry]
  }

getBalanceBreakdown ::
  (BeamFlow m r, CacheFlow m r) =>
  AccountRef ->              -- Id Account, or (CounterpartyType, counterpartyId, AccountClass)
  m BalanceBreakdown         -- cutoff and minRetained come from getAccountConfig, not args

-- AllMoney / PartialMoney, resolved INSIDE the kernel's balance lock:
data TransferAmount = AllAvailable | Exact HighPrecMoney

resolveTransferAmount ::
  ResolvedAccountConfig -> BalanceBreakdown -> TransferAmount ->
  Either FinanceError (HighPrecMoney, HighPrecMoney)   -- (gross, fee)

-- Kernel-owned lock (replaces makeWalletRunningBalanceLockKey / feeBalanceLockKey):
withAccountBalanceLock :: (CacheFlow m r) => Id Account -> Int -> m a -> m a
-- key: "Finance:BalanceLock:" <> accountId
```

**Negative-balance enforcement** goes into `Ledger.Service.createEntryWithBalanceUpdate`:
after computing the from-side ending balance, if it is negative and the from-account's
resolved config has `allowNegativeBalance = False`, fail with
`AccountError InsufficientBalance` (the error that exists today and is never raised).
Rollout: ship with every existing class defaulting to `True` (today's behaviour), then flip
per class via config rows — no big-bang risk.

---

## 5. The Transfer primitive

### 5.1 Canonical status — the ONE money lifecycle

```haskell
-- Lib.Finance.Transfer.Types
data TransferStatus
  = CREATED        -- row exists; nothing external has happened (also: scheduled, not yet due)
  | SUBMITTED      -- handed to the rail; no acknowledgement yet (crash window marker)
  | IN_FLIGHT      -- rail acknowledged, outcome unknown (Juspay PROCESSING/DEBITED/AWAITING_APPROVAL…)
  | NEEDS_REVIEW   -- rail says ambiguous/manual (MANUAL_REVIEW, CONFLICTED, bulk ambiguous-send)
  | SUCCEEDED      -- terminal; ledger settled
  | FAILED         -- terminal; ledger compensated (hold reversed / pending legs voided)
  | REVERSED       -- was SUCCEEDED, rail later reversed (Juspay REVERSED); ledger re-compensated
  deriving (Eq, Ord, Show, Read, Generic, ...)

-- What a webhook / poll / submit result means, rail-neutrally:
data TransferOutcome
  = OutcomeSucceeded
  | OutcomeFailed Text          -- reason
  | OutcomePending              -- still in flight; bump railStatus only
  | OutcomeNeedsReview Text
  | OutcomeReversed Text
```

Terminal = `SUCCEEDED | FAILED | REVERSED`. Valid transitions are enforced in
`Transfer/Outcome.hs` (one place), including the audited-today special case: an
`OutcomeSucceeded` arriving after `FAILED` re-opens the hold and settles (exactly what
`settlePayoutLedger` supports), recorded as `FAILED → SUCCEEDED` with an audit entry.

The raw rail status is **never** interpreted outside the rail record. It is stored verbatim:

```haskell
data Transfer = Transfer
  { id              :: Id Transfer
  , direction       :: TransferDirection      -- INTERNAL | OUTBOUND | INBOUND
  , status          :: TransferStatus
  , railKey         :: Maybe Text             -- Nothing for INTERNAL
  , railStatus      :: Maybe Text             -- opaque passthrough, e.g. "FULFILLMENTS_SUCCESSFUL"
  , railOrderId     :: Maybe Text             -- PayoutOrder/PaymentOrder id at the rail layer
  , accountId       :: Id Account             -- the finance account this transfer is about
  , counterAccountId :: Maybe (Id Account)    -- INTERNAL only
  , requestedAmount :: Maybe HighPrecMoney    -- Nothing = AllAvailable was requested
  , amount          :: HighPrecMoney          -- resolved gross
  , feeAmount       :: HighPrecMoney
  , currency        :: Currency
  , referenceType   :: Text                   -- domain's ref vocabulary ("WalletPayout", "WalletTopup", …)
  , referenceId     :: Text                   -- idempotency anchor (domain's request id)
  , holdEntryId     :: Maybe (Id LedgerEntry)
  , entryIds        :: [Id LedgerEntry]       -- all legs, persisted (kills the Redis stash)
  , metadata        :: TransferMetadata
  , scheduledFor    :: Maybe UTCTime
  , attemptCount    :: Int
  , lastError       :: Maybe Text
  , merchantId, merchantOperatingCityId, createdAt, updatedAt
  }

-- Flow artifacts ONLY — no domain data. The counterparty on the account is the identity.
data TransferMetadata = TransferMetadata
  { utr            :: Maybe Text
  , feeBreakdown   :: Maybe FeeBreakdown
  , coveredRefs    :: Maybe [CoveredRef]      -- [{refType, amount}] — which earnings this payout covers
  , reason         :: Maybe Text
  , railExtra      :: Maybe Value             -- rail-specific blob (batch id, fulfillment method…)
  }
data CoveredRef = CoveredRef { refType :: Text, amount :: HighPrecMoney }
```

`finance_transfer` is the idempotency anchor: unique on `(referenceType, referenceId)`.
`postOnceByReference` remains as the per-leg guard underneath.

### Why a new table and not `finance_ledger_entry`?

Because a ledger entry is a **fact** and a transfer is a **process**, and the two have
opposite storage disciplines:

1. **Mutability.** The ledger is append-only (LAW 2: history is immutable) — entries are
   never updated except narrowly-scoped settlement stamps. A transfer mutates for hours
   or days: status, railStatus, railOrderId, attemptCount, lastError. Pushing that churn
   into the journal would mean a dozen mutable columns on the immutable table.
2. **Cardinality.** One transfer ↔ many entries (hold, settlement, reversal, fee legs,
   credit-recipe legs) plus N covered earning entries. There is no single entry row that
   *is* the transfer; you'd have to elect one leg as the carrier — which breaks when the
   hold is posted late (SubmitThenHold) or never (rejected before any posting).
3. **A transfer can exist with zero entries.** Scheduled-but-not-due, inbound topups
   awaiting payment, submissions rejected before the hold. The ledger cannot represent
   "intent, nothing moved yet" without fake zero-entries.
4. **Idempotency anchor.** Unique `(referenceType, referenceId)` needs a
   one-row-per-process table; entries are many-per-reference by design.
5. **Query shape.** Ops wants status scans ("all NEEDS_REVIEW", "stuck IN_FLIGHT") on a
   small hot table — not index pressure and scan workloads on the ever-growing journal.
6. **Precedent.** This is exactly the role `PayoutRequest` / `PaymentOrder` /
   `WalletTransaction` play today — the unification collapses those process tables into
   one kernel-owned one; it does not move process state into the journal.

The two stay linked both ways: `transfer.entryIds`/`holdEntryId` point at the legs, and
settled entries carry `settlementId = transferId`.

### 5.2 Rails — injected, registry-dispatched

```haskell
-- Lib.Finance.Transfer.Rail
data HoldOrdering = HoldThenSubmit | SubmitThenHold
  -- HDFC bulk: HoldThenSubmit (full compensation ladder)
  -- Juspay/Stripe: SubmitThenHold — but with the fix: a failed hold after submit now
  --   flips the transfer to NEEDS_REVIEW instead of today's logError-and-forget.

data RailSubmitResult
  = RailAccepted   { railOrderId :: Text, railStatus :: Text }
  | RailRejected   { reason :: Text }        -- confirmed rejection → compensate now
  | RailAmbiguous  { reason :: Text }        -- unknown outcome → NEEDS_REVIEW, poll later
  | RailDeferred   { railStatus :: Text }    -- accepted locally, sent later (HDFC bulk file)

data TransferRail m = TransferRail
  { railKey        :: Text                                  -- "JUSPAY_PAYOUT", "STRIPE_PAYOUT", "HDFC_BULK", "JUSPAY_PG", …
  , holdOrdering   :: HoldOrdering
  , submit         :: RailSubmission -> m RailSubmitResult  -- wraps createPayoutService / createOrderService
  , pollStatus     :: Transfer -> m (Maybe Text)            -- fresh raw railStatus, Nothing = can't poll (bulk items)
  , classifyStatus :: Text -> TransferOutcome               -- THE one rail→canonical mapping for this rail
  , collectPayload :: Maybe (RailSubmission -> m Value)     -- INBOUND only: SDK payload for the app client
  }

data RailSubmission = RailSubmission
  { transferId :: Id Transfer
  , amount     :: HighPrecMoney
  , currency   :: Currency
  , accountId  :: Id Account
  , counterpartyType :: CounterpartyType
  , counterpartyId   :: Text
  , metadata   :: TransferMetadata
  }

type RailRegistry m = Map.Map Text (TransferRail m)   -- app-side, like reconciliationRegistry
lookupRail :: RailRegistry m -> Text -> Maybe (TransferRail m)
```

The rails are **built in the apps** out of `lib/payment` functions (`createPayoutService`,
`executePayoutRequestInternal`, `createOrderService`, `payoutStatusService`), exactly as
`reconciliationRegistry` is built today. `classifyStatus` is written **once per rail**,
deleting the ≥6 duplicate mapping tables
(`castPayoutOrderStatusToPayoutRequestStatus`, `castPayoutOrderStatus`, `castOrderStatus`,
`castRefundRequestStatus`, `refundStatusToRequestStatus`, `requestPayoutStatus`/`orderPayoutStatus`).

### 5.3 The verbs

```haskell
-- ───────────── Internal: FA → FA ─────────────
-- Lib.Finance.Transfer.Internal
-- Instant settled entries; a Transfer row is written for history, status jumps CREATED→SUCCEEDED.
transferInternal ::
  (BeamFlow m r, CacheFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  InternalTransferSpec ->
  m (Either FinanceError Transfer)

data InternalTransferSpec = InternalTransferSpec
  { ctx            :: FinanceCtx
  , fromRole       :: AccountRole
  , toRole         :: AccountRole
  , amount         :: TransferAmount        -- AllAvailable resolved under the from-account lock
  , referenceType  :: Text
  , metadata       :: TransferMetadata
  }

-- ───────────── Outbound: FA → real account (payout) ─────────────
-- Lib.Finance.Transfer.Outbound
initiateTransferOut ::
  (BeamFlow m r, CacheFlow m r, HasActorInfo m r, MonadCatch m) =>
  RunFinance m ->
  TransferRail m ->
  OutboundSpec ->
  m (Either FinanceError Transfer)

data OutboundSpec = OutboundSpec
  { ctx           :: FinanceCtx
  , amount        :: TransferAmount   -- AllAvailable = redeemable − holds − minRetained − fee
  , referenceType :: Text             -- e.g. "WalletPayout"
  , referenceId   :: Text             -- domain request id; idempotency anchor
  , instant       :: Bool             -- checked against instantTransferOutEnabled
  , metadata      :: TransferMetadata
  }

-- Internal sequence (all under withAccountBalanceLock):
--   1. getAccountConfig → enforce transferOutEnabled / instant / minTransferOutAmount /
--      maxTransferOutPerDay (counted from finance_transfer, not reversal-prefix string parsing)
--   2. getBalanceBreakdown → resolveTransferAmount (fee from config)
--   3. upsert Transfer row (CREATED) — duplicate (refType, refId) returns the existing row
--   4. per rail.holdOrdering:
--        HoldThenSubmit: post hold (transferInProcessing OwnerLiability→OwnerPayoutLiability),
--                        mark covered entries PROCESSING, then rail.submit
--        SubmitThenHold: rail.submit, then post hold; hold failure ⇒ NEEDS_REVIEW (not logError)
--   5. map RailSubmitResult:
--        RailAccepted  → SUBMITTED/IN_FLIGHT (+railStatus, railOrderId)
--        RailRejected  → compensate (reverse hold, release entries) → FAILED
--        RailAmbiguous → NEEDS_REVIEW (hold kept — money must not be double-spendable)
--        RailDeferred  → SUBMITTED (bulk; batch status check will resolve)
--   The compensation ladder (today's giveMoneyBack/stopNotSent with HOLD_FAILED/SEND_FAILED/
--   REVERSAL_PENDING codes) moves here verbatim and now covers BOTH orderings.

-- ───────────── Inbound: real account → FA (recharge/topup) ─────────────
-- Lib.Finance.Transfer.Inbound
initiateTransferIn ::
  (BeamFlow m r, CacheFlow m r, HasActorInfo m r) =>
  TransferRail m ->
  InboundSpec ->
  m (Either FinanceError (Transfer, Maybe Value))   -- Value = SDK payload for the client

data InboundSpec = InboundSpec
  { ctx           :: FinanceCtx
  , amount        :: HighPrecMoney       -- inbound is always exact
  , referenceType :: Text                -- e.g. "WalletTopup"
  , referenceId   :: Text
  , creditLegs    :: CreditRecipe        -- what to post on success (see below)
  , metadata      :: TransferMetadata
  }

-- The success posting is DECLARED up front, not hand-coded in the webhook handler:
data CreditRecipe = CreditRecipe
  { legs    :: [(AccountRole, AccountRole, AmountPortion, Text)]  -- from, to, portion, refType
  , invoice :: Maybe InvoiceConfig
  }
data AmountPortion = FullAmount | Portion HighPrecMoney | Remainder
-- driver topup:    CreditRecipe [ (PlatformAsset, OwnerLiability, FullAmount, "WalletTopup") ] (Just inv)
-- prepaid topup:   the 4-leg GST/liability/TDS/credit split from Prepaid.creditPrepaidBalance,
--                  expressed as Portions — finally through FinanceM, idempotent via the Transfer row.
-- rider deposit:   [ (BuyerAsset, BuyerExternal, FullAmount, _), (BuyerExternal, OwnerLiability, FullAmount, "BOOKING_DEPOSIT_TOPUP") ]

-- ───────────── Outcome: the single idempotent handler ─────────────
-- Lib.Finance.Transfer.Outcome   (absorbs Ledger/PayoutSettlement.hs)
applyTransferOutcome ::
  (BeamFlow m r, CacheFlow m r, HasActorInfo m r) =>
  RunFinance m ->
  Id Transfer ->
  TransferOutcome ->
  m (Either FinanceError Transfer)
-- OUTBOUND Succeeded: ensure hold (post if initiate never did — crash-window repair),
--   settle OwnerPayoutLiability→settlementToRole, markEntriesAsClaimed, settle coveredRefs
--   entries PAID_OUT with settlementId = transferId, post fee legs per config bearer.
-- OUTBOUND Failed: reverse active hold, mark claimed, release covered entries to UNSETTLED.
-- OUTBOUND Reversed: like Failed but from SUCCEEDED, with a fresh reversal pair.
-- INBOUND Succeeded: post CreditRecipe legs + invoice (idempotent via transfer row).
-- INBOUND Failed: nothing posted; status only.
-- Pending/NeedsReview: bump status + railStatus only.
-- Every path is re-runnable; webhook, poll job, bulk check, and dashboard can all call it.

-- Convenience for webhook handlers — decode rail payload domain-side, then:
applyRailStatus ::
  ... => RunFinance m -> TransferRail m -> Id Transfer -> Text {- raw railStatus -} ->
  m (Either FinanceError Transfer)
-- = stamp railStatus, applyTransferOutcome (rail.classifyStatus raw)

-- ───────────── Status check (poll) ─────────────
checkTransferStatus ::
  ... => RunFinance m -> TransferRail m -> Id Transfer -> m (Either FinanceError Transfer)
-- rail.pollStatus → applyRailStatus. The generic poll-job loop (interval, maxAttempts from
-- config) is provided as runTransferStatusCheckJob — the shape of today's
-- Lib.Payment.Payout.StatusCheck.Handle, but kernel-owned and rail-neutral.

-- ───────────── Scheduled transfers ─────────────
-- Lib.Finance.Transfer.Scheduled
-- scheduled_transfer_rule: keyed (merchantOperatingCityId, accountClass, name)
--   frequency DAILY|WEEKLY|MONTHLY|HOURLY|EVERY_N_DAYS, timeOfDay, dayOfWeek/Month,
--   intervalDays/Hours, timeDiffFromUtc, minimumAmount, batchSize, maxRetries,
--   rescheduleBufferMinutes, isEnabled, defaultRailKey
computeNextRunTime :: ScheduledTransferRule -> UTCTime -> UTCTime   -- moves verbatim from ScheduledBatchPayout.hs
runScheduledSweep ::
  ... =>
  RunFinance m -> RailRegistry m -> ScheduledTransferRule ->
  (Int -> Maybe Text -> m ([AccountSweepTarget], Maybe Text)) ->  -- INJECTED enumeration (domain
  m SweepOutcome                                                  -- knows who is eligible; kernel
                                                                  -- does NOT learn "driver")
data AccountSweepTarget = AccountSweepTarget { accountId :: Id Account, ctx :: FinanceCtx }
-- paging cursor, per-target initiateTransferOut AllAvailable, backoff — all kernel; the
-- Allocator job shell stays in the app, ~30 lines.

-- ───────────── History ─────────────
-- Lib.Finance.Transfer.History
getAccountHistory ::
  ... => AccountRef -> HistoryFilter -> Int -> Int -> m [AccountHistoryItem]
-- one feed of: internal transfers, outbound (virtual→real), inbound (real→virtual),
-- adjustments, reversals — Transfer rows joined with their ledger legs.

getSettledLedgerBySettlement ::
  ... => Text -> Text -> Int -> Int -> m [LedgerEntry]
-- refType -> settlementId (= transferId for payouts) -> offset -> limit.
-- (the user-requested getSettledLedgerWithId; thin formalization over findBySettlementId)
```

### 5.4 What stays domain-side, permanently

- **Which account classes exist and what they mean** (wallet, prepaid, deposit) — mapping only.
- **Eligibility that isn't about money**: VPA present, person not blocked, roles, referral
  thresholds, device checks, cash-ride gating. Domains gate *before* calling the verbs.
- **Notifications** (FCM/SMS on transfer events) — domains observe the returned
  `Transfer`/status change; optionally a domain-injected `onStatusChange :: Transfer ->
  TransferStatus -> m ()` hook on `applyTransferOutcome`, same shape as PostActions.
- **Webhook HTTP endpoints + signature verification + payload decode** — then `applyRailStatus`.
- **Cash settlement of payouts** (`CASH_PAID`/`CASH_PENDING`): modelled as the domain choosing
  `transferInternal` (wallet → platform cash control) instead of an outbound rail transfer;
  the domain may keep a thin presentation enum derived from which path was taken.
- **PostActions** (wallet-gated tier recheck): unchanged; everything goes through the app's
  `runFinance`, which the verbs receive injected.

---

## 6. Status ownership — what gets deleted

| Enum (today) | Fate |
|---|---|
| `PayoutRequestStatus` (10, lib/payment) | Lifecycle → `TransferStatus`. `CASH_PAID/CASH_PENDING/EXCLUDED` → domain presentation derived from transfer path. Table retained short-term as rail-layer artifact, status column derived. |
| `ScheduledPayoutStatus` (9, driver) + `PayoutStatusHistory` | Deleted → `finance_transfer.status` + kernel `StateTransition` audit. |
| `WalletTransactionStatus` (6, driver) | Deleted → INBOUND `Transfer`. (Also fixes `fromMaybe DWT.SUCCESS` — a missing row is no longer reported as a successful topup.) |
| `PayoutStatus` (7, DailyStats + lib/payment) | Frozen legacy (DailyStats referral flow) until that flow adopts transfers, then deleted. |
| `PayoutOrderStatus` (24, Juspay) / `TransactionStatus` (13) | Stay at the rail layer; appear in kernel only as opaque `railStatus` text. |
| `PayoutBatchStatus` (7) | Stays in lib/payment (batch mechanics are rail plumbing); items resolve via `applyRailStatus`. |
| `CashbackStatus` (metro, 5) | Derived view of `TransferStatus` once metro cashback adopts transfers. |
| `SettlementStatus` (kernel UNSETTLED/PROCESSING/PAID_OUT) | Unchanged — it's the per-entry claim state the transfer machinery drives. |
| 6+ rail→domain mapping tables | Deleted → one `classifyStatus` per rail record. |

---

## 7. Adoption plan (phased; each phase ships alone)

**Phase 0 — Config + balance (no behaviour change).**
`FinanceAccountConfig` table + `getAccountConfig` ladder + config-pilot registration;
`getBalanceBreakdown`; `InsufficientBalance` enforcement behind `allowNegativeBalance`
(seeded `True` everywhere); delete dead `FinanceEnv/ConfigKey/ConfigValue`. Driver wallet
reads (`computePayoutableBalance`, deposit `getAvailableBalance`) switch to
`getBalanceBreakdown`; config rows seeded from `DriverWalletConfig` values, which become
deprecated-read-only.

**Phase 1 — Outbound transfer; driver wallet payout first.**
`finance_transfer` + `Transfer/{Types,Rail,Outbound,Outcome}.hs`; driver-app builds
`JUSPAY_PAYOUT`/`STRIPE_PAYOUT`/`HDFC_BULK` rail records; `initiateWalletPayoutWith`,
`postOwnerPayoutLiability`, the `DRIVER_WALLET_TRANSACTION` settlement branch, the Redis
entry-id stash, and the per-order poll job body are replaced. `PayoutSettlement.hs`
becomes internal to `Outcome.hs` (rider cashback keeps compiling via it until Phase 2).
This phase also fixes the two audited bugs: unheld SubmitThenHold window → `NEEDS_REVIEW`;
payout fee counted via transfer rows instead of reversal-reason string prefixes.

**Phase 2 — Rider cashback + inbound.**
`Transfer/Inbound.hs` + `CreditRecipe`; driver topup webhook credit and airport cash
recharge move to INBOUND transfers; rider `CashbackPayout.hs`/`runRiderPayoutSettlement`
RIDE_OFFER_CASHBACK branch move to outbound verbs; legacy
`releaseWalletEntriesReservation` / `releaseCashbackEntriesReservation` shims deleted.

**Phase 3 — Scheduled rules.**
`scheduled_transfer_rule` + `Transfer/Scheduled.hs`; `ScheduledBatchPayout` job becomes a
thin shell around `runScheduledSweep` with injected driver/fleet enumeration;
`ScheduledPayoutConfig` migrated (vehicleCategory moves into the enumeration, where it
belongs — it's "who", not "how").

**Phase 4 — Prepaid onto FinanceM/transfers.**
`Prepaid.creditPrepaidBalance`/`debitPrepaidBalance*` restated as `CreditRecipe` inbound +
FinanceM verbs under `(RideCredit, DRIVER, subLedger=PREPAID)`; kills the 9 raw
`createEntryWithBalanceUpdate` calls and hand-rolled idempotency.

**Phase 5 — Bring the unledgered flows in (highest new value).**
Referral/backlog awards, metro cashback → outbound transfers (gaining a ledger they've
never had); FRFS/ticket/parking refunds evaluated for INBOUND-reversal transfers vs staying
on FRFSRecon. Then delete the frozen legacy enums.

Known debts the design settles (from the audits): LedgerAdjustment's PostActions bypass
(verbs take the injected `runFinance`, so admin adjustments can't skip dispatch);
`createWalletEntryDelta`'s manual bypass (replaced by `transferInternal`); `createReversal`
dedup hand-rolled in rider refunds (Outcome.hs owns reverse-once); `PayoutAmbiguous`
permanent holds (NEEDS_REVIEW is a first-class, queryable state for ops).

## 8. Open questions

1. `finance_transfer` write volume = every topup+payout+internal transfer; ride-fare legs stay
   pure ledger (no Transfer row) — confirm we don't want per-ride transfer rows for history UX.
2. `maxTransferOutPerDay` day boundary: reuse `redeemableCutoff` tz or city tz? (Today:
   `timeDiffFromUtc` hacks.)
3. Does dashboard ops need a manual `forceTransferOutcome` verb (bounded override for
   NEEDS_REVIEW), or do we keep the existing PayoutHoldRelease surface rebuilt on transfers?
