# Nammayatri Finance Module — Driver-Side Guide

A practical walk-through of the double-entry accounting model that powers the
driver wallet, earnings, payouts, and the BPP (driver-app) ride lifecycle.

---

## 1. Double-Entry Bookkeeping — Primer

### The two laws the kernel enforces

1. **Conservation of money** — every movement of money touches **exactly two
   accounts**: one is **debited** (`fromAccount`), the other is **credited**
   (`toAccount`). The balance sheet never goes out of balance.
2. **Immutability of history** — entries are never edited or deleted. To undo,
   a **reversal entry** is written with swapped sides (`createReversal`,
   `Backend/lib/finance-kernel/src/Lib/Finance/Ledger/Service.hs:179`).

### The "golden rules" — how debit/credit change a balance

This is the only table you need to memorise. It is implemented literally in
`createEntryWithBalanceUpdate`
(`Backend/lib/finance-kernel/src/Lib/Finance/Ledger/Service.hs:136-144`):

| Account Type                                             | Debit (`from` side) | Credit (`to` side) |
| -------------------------------------------------------- | ------------------- | ------------------ |
| **Asset**, **Expense**                                   | balance **+=** amt  | balance **-=** amt |
| **Liability**, **Revenue**, **External**, **RideCredit** | balance **-=** amt  | balance **+=** amt |

```haskell
-- Backend/lib/finance-kernel/src/Lib/Finance/Ledger/Service.hs
-- LAW 1: Credits = Debits (fromAccount debited, toAccount credited)
fromEndBal =
  if isAssetOrExpenseAccount fromAccount
    then fromStartBal + amount    -- DR asset/expense  -> grows
    else fromStartBal - amount    -- DR liability/rev  -> shrinks
toEndBal =
  if isAssetOrExpenseAccount toAccount
    then toStartBal - amount      -- CR asset/expense  -> shrinks
    else toStartBal + amount      -- CR liability/rev  -> grows
```

### Why does debiting an asset _grow_ it? (the intuition)

The single biggest source of confusion: **"debit" and "credit" are not
synonyms for "decrease" and "increase"**. They just mean **"entry on the left
column of a T-account"** and **"entry on the right column of a T-account"**.
The sign they impart depends on which side of the accounting equation the
account lives on.

#### The one equation that explains everything

```
Assets + Expenses  =  Liabilities + Equity + Revenue
     (left side)            (right side)
```

Every transaction must keep this equation balanced, so the convention is:

- **Left-side accounts (Assets, Expenses)** → grow with a **debit** (a
  left-side entry), shrink with a credit.
- **Right-side accounts (Liabilities, Revenue, Equity)** → grow with a
  **credit** (a right-side entry), shrink with a debit.

That is the _entire_ rule. "A debit grows an asset" is the same statement as
"assets live on the left side of the equation".

#### The "which bucket?" picture

Think of the balance sheet as two big buckets — Left (what the business
**has or spent**) and Right (**where it came from**). An asset is something
the business owns, so it belongs in the Left bucket. To add to the Left
bucket, you write on the left column — we call that a debit. Nothing
mysterious: _debit_ just = _entry on the left column of the T-account_.

```
   CASH (Asset)                 LOAN (Liability)
  DR  |  CR                    DR  |  CR
  +100|                            |+100
```

Same ₹100 transaction, written on the debit side of Cash and the credit side
of Loan. Both "grew" — one via its debit column, one via its credit column —
because that is **their natural home**.

#### Every transaction has two sides because money came from somewhere

If an asset grew by ₹100, one of three things must be true:

- Something else on the left **shrank** by ₹100 (e.g. you spent cash to buy a
  laptop — Cash down, Equipment up), or
- Something on the right **grew** by ₹100 (e.g. you took a loan, or earned
  revenue).

So debit-Asset and credit-Liability showing up together in the same entry is
not a coincidence — it's the law. "Cash went up by 100 and the loan went up
by 100" is the **same event** written from two angles: what happened to our
stuff (left), and where it came from (right).

#### The bank-statement paradox

When your bank statement shows a deposit as a **"credit"** it feels
backwards — your balance went up, shouldn't that be a debit? The trick is
that **the bank statement is the bank's books, not yours.**

- From _your_ books: your bank balance is an **Asset**. A deposit debits it →
  balance grows. ✓
- From _the bank's_ books: your deposit is a **Liability** (they owe you that
  money back). A deposit credits that liability → their "we owe you" balance
  grows. ✓

Both are correct. Once you realise the word "credit" on a bank statement is
the bank describing _its own_ liability account, the confusion disappears.

#### Mnemonic (if you want one)

**DEAD CLIC** — **D**ebits increase **E**xpenses, **A**ssets, **D**rawings;
**C**redits increase **L**iabilities, **I**ncome (Revenue), **C**apital
(Equity). Ugly, but it sticks.

### A tiny worked example (ignore the platform for now)

You deposit ₹1000 cash into a bank account.

```
transfer  BankCash (Asset)  ->  CustomerDeposits (Liability)   amount=1000

fromAccount = BankCash           (Asset)     : 0    + 1000 = 1000
toAccount   = CustomerDeposits   (Liability) : 0    + 1000 = 1000
```

Both sides grew and the balance sheet still balances:
`Assets (1000) = Liabilities (1000) + Equity (0)`. That is exactly what
double-entry is supposed to look like — it is **not** a typo that both numbers
went up.

### A platform example (from the actual code)

When a rider finishes a ₹100 online-paid ride:

```
transfer  BuyerAsset (Asset)  ->  OwnerLiability (Liability)  amount=100  ref=BaseRide

BuyerAsset      (Asset)     :  x     + 100       # cash received from PG
OwnerLiability  (Liability) :  y     + 100       # platform now owes driver
```

We received cash **and** we incurred a liability to the driver at the same
time. Same shape as the bank example above.

---

## 2. Account Types — What Each One Is, In Accounting Terms

### 2.1 The primitive enum

Defined in
`Backend/lib/finance-kernel/src-read-only/Lib/Finance/Domain/Types/Account.hs:30`:

```haskell
data AccountType
  = Asset        -- cash & receivables the platform controls
  | Liability    -- IOUs (driver wallet, govt dues, PG payables)
  | Revenue      -- platform income (commission, SaaS fees)
  | Expense      -- platform outflows (incentives, PG fees)
  | External     -- clearing / in-transit bucket (credit-normal)
  | RideCredit   -- prepaid ride inventory (credit-normal)
```

`Asset` and `Expense` are **debit-normal** (a debit grows them). Everything
else — `Liability`, `Revenue`, `External`, `RideCredit` — is **credit-normal**
(a credit grows them). That single fact drives every balance update in the
kernel.

### 2.2 The practical roles (`AccountRole`)

Business code never touches `AccountType` directly. It uses named **roles** in
`Backend/lib/finance-kernel/src/Lib/Finance/FinanceM.hs:237-419`, which the
`account` function resolves into a concrete ledger account using the current
`FinanceCtx`. Here is the full mapping — treat this as the cheat sheet:

| Role                  | Accounting Category | Counterparty         | Plain-English meaning                          |
| --------------------- | ------------------- | -------------------- | ---------------------------------------------- |
| `BuyerAsset`          | **Asset**           | BUYER (merchant)     | Cash the platform holds "on behalf of buyer"   |
| `BuyerExternal`       | **External**        | BUYER (merchant)     | In-transit / PG clearing bucket                |
| `OwnerLiability`      | **Liability**       | DRIVER / FLEET_OWNER | **Driver wallet** — what we owe the driver     |
| `OwnerExpense`        | **Expense**         | DRIVER / FLEET_OWNER | Driver-side expense (rare, reporting)          |
| `PrepaidOwner`        | **RideCredit**      | DRIVER / FLEET_OWNER | Prepaid-plan ride credits                      |
| `SellerAsset`         | **Asset**           | SELLER (platform)    | Platform's own cash/receivable                 |
| `SellerLiability`     | **Liability**       | SELLER (platform)    | Platform's own payable                         |
| `SellerRevenue`       | **Revenue**         | SELLER (platform)    | **Commission income**, SaaS, cancellation keep |
| `SellerRideCredit`    | **RideCredit**      | SELLER (platform)    | Platform-issued ride credits                   |
| `GovtIndirect`        | **Liability**       | GOVERNMENT_INDIRECT  | **GST / VAT** collected, owed to tax authority |
| `GovtDirect`          | **Liability**       | GOVERNMENT_DIRECT    | **TDS** withheld, owed to tax authority        |
| `GovtDirectAsset`     | Asset               | GOVERNMENT_DIRECT    | TDS receivable (when we are the taxpayer)      |
| `GovtDirectExpense`   | Expense             | GOVERNMENT_DIRECT    | Direct tax expense                             |
| `ParkingFeeRecipient` | **Liability**       | AIRPORT              | Owed to airport/parking authority              |
| `PGPaymentExpense`    | Expense             | PG_PAYMENT_JUSPAY    | PG fees we pay on incoming payments            |
| `PGPaymentLiability`  | Liability           | PG_PAYMENT_JUSPAY    | Amount in PG settlement pipeline               |
| `PGPayoutExpense`     | Expense             | PG_PAYOUT_JUSPAY     | PG fees we pay on driver payouts               |
| `PGPayoutLiability`   | Liability           | PG_PAYOUT_JUSPAY     | Amount in PG payout pipeline                   |
| `PGGstAsset`          | Asset               | GOVERNMENT_INDIRECT  | Input GST credit on PG fees                    |

### 2.3 The `transfer` API

All business code speaks in roles, not accounts:

```haskell
-- Backend/lib/finance-kernel/src/Lib/Finance/FinanceM.hs:428
transfer :: AccountRole -> AccountRole -> HighPrecMoney -> Text -> FinanceM m (Maybe (Id LedgerEntry))
transfer fromRole toRole amount refType = ...
```

`FinanceM` resolves each role to an account (`getOrCreateAccount`), creates the
ledger entry, updates both balances atomically, and appends the entry id to a
`collectedEntryIds` list that the caller can later hand to the invoice and to
`markEntriesAsPaidOut`.

---

## 3. Ledger Entry Anatomy

`Backend/lib/finance-kernel/src-read-only/Lib/Finance/Domain/Types/LedgerEntry.hs`

| Field                                                       | Purpose                                                                      |
| ----------------------------------------------------------- | ---------------------------------------------------------------------------- |
| `fromAccountId`, `toAccountId`                              | The two accounts this entry touches (debit, credit)                          |
| `amount`, `currency`                                        | The movement                                                                 |
| `entryType`                                                 | `Expense` / `Revenue` / `LiabilityCreated` / `LiabilitySettled` / `Reversal` |
| `status`                                                    | `PENDING` / `DUE` / `SETTLED` / `VOIDED`                                     |
| `settlementStatus`                                          | `UNSETTLED` / `PAID_OUT` — used for driver disbursements                     |
| `settlementId`                                              | `PayoutRequest` id once paid out                                             |
| `referenceType`, `referenceId`                              | "BaseRide" / "Commission" / "WalletPayout" + booking id etc.                 |
| `reversalOf`                                                | Points back to the original entry if this is a reversal                      |
| `fromStartingBalance` / `fromEndingBalance` (same for `to`) | **Balance snapshot** captured at write time — audit trail                    |

The balance snapshots are the reason we can reconstruct the driver's wallet at
any moment in history without recomputing from scratch.

---

## 4. The Driver Wallet — How the Balance Is Stored and Surfaced

### 4.1 Where the running balance lives

Each driver has **one** `Account` row with `accountType = Liability` and
`counterpartyType = DRIVER` (or `FLEET_OWNER`). That single row is the
**source of truth** — `account.balance` is updated atomically on every write
via `QAccount.updateBalance`
(`Backend/lib/finance-kernel/src/Lib/Finance/Ledger/Service.hs:175-176`).

Lookup helper in
`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Finance/Wallet.hs:202`:

```haskell
getWalletAccountByOwner counterpartyType ownerId = do
  accounts <- findAccountsByCounterparty (Just counterpartyType) (Just ownerId)
  pure $ find (\acc -> acc.accountType == Liability) accounts
```

The wallet UI calls `getWalletBalanceByOwner` which just returns `acc.balance`.

### 4.2 Redeemable vs. non-redeemable

Not every rupee in the wallet can be withdrawn immediately. Recent credits
(within a `payoutCutOffDays` window) are locked to absorb reversals and
fraud review. The split is computed on demand:

```
redeemable      = max 0 (currentBalance − nonRedeemableBalance)
nonRedeemable   = sum of credit entries whose timestamp > cutoff
```

`getNonRedeemableBalance`
(`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Finance/Wallet.hs:188`)
issues a DB-level filter using `findCreditsByAccountAfterTime`, so we do not
load the whole history.

### 4.3 DailyStats is **not** the source of truth

`DailyStats` is a denormalised reporting rollup (total earnings, tips, toll
etc.). It drives the driver dashboard cards, but **never** re-derive a balance
from it — always go through the ledger account.

---

## 5. Driver Ride Flow — What the Ledger Does at Each Step

### 5.1 Assignment (driver accepts a ride)

**Ledger writes: _none_.** This is a deliberate choice and worth understanding:

- At assignment time no money has actually moved. The fare is an estimate, the
  commission is unknown, and the ride might still be cancelled.
- The kernel has a `PENDING` entry status for holds, but the BPP flow **does
  not create one at assignment**. It trusts the ride-status state machine and
  only writes ledger entries at terminal events.
- Reasoning: keeping the ledger append-only and monotonic is easier to audit
  than a stream of PENDING → REVERSED → SETTLED records, most of which would
  cancel out.

Wallet effect on the driver app: **unchanged**. The UI shows the ride in
progress but `account.balance` has not moved.

### 5.2 Completion (driver ends the ride)

Entry point:
`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/EndRide/Internal.hs:460`
→ `createDriverWalletTransaction`, called asynchronously from
`endRideTransaction` under a Redis lock
(`makeWalletRunningBalanceLockKey driverId`, 10 s).

The core sequence for a ₹100 online-paid ride (GST ₹5, toll ₹15, parking ₹10,
commission ₹10, TDS 1% on base):

```haskell
-- Step 1: money in — run once per component
--   (BuyerAsset -> BuyerExternal) is an intermediate "in-transit" booking
--    so the PG clearing leg is visible in reports; no entry id is collected.
transfer_ BuyerAsset     BuyerExternal   100  "BaseRide"
transfer  BuyerExternal  OwnerLiability  100  "BaseRide"        -- driver wallet +100

transfer_ BuyerAsset     BuyerExternal     5  "GSTOnline"
transfer  BuyerExternal  GovtIndirect      5  "GSTOnline"        -- govt payable

transfer_ BuyerAsset     BuyerExternal    15  "TollCharges"
transfer  BuyerExternal  OwnerLiability   15  "TollCharges"     -- driver wallet +15

transfer_ BuyerAsset     BuyerExternal    10  "ParkingCharges"
transfer  BuyerExternal  OwnerLiability   10  "ParkingCharges"  -- driver wallet +10

-- Step 2: deductions from the driver wallet
transfer OwnerLiability  GovtDirect        1  "TDSDeductionOnline"  -- driver wallet -1
transfer OwnerLiability  SellerRevenue    10  "Commission"          -- driver wallet -10

-- Step 3: bundle everything into an invoice with the collected entry ids
invoice invoiceConfig
```

#### The `BuyerAsset → BuyerExternal → OwnerLiability` two-step, decoded

Newcomers always ask the same question: **"if `BuyerAsset` is growing here,
shouldn't there be a prior entry that records the rider actually paying the
cash?"** The answer requires unlearning a common misconception.

**There is no prior entry, and there does not need to be one.** In
double-entry, the "where did the money come from?" question is answered by
the **credit leg of the same entry**, not by a separate earlier entry. Every
single row in the ledger is already balanced — it has a debit and a credit,
both visible in one line.

So in Step A:

```haskell
transfer_ BuyerAsset  BuyerExternal  100  "BaseRide"
--         ^debit       ^credit
```

this _is_ the entry that records cash arrival. The debit leg says "Cash
(BuyerAsset) grew by 100" and the credit leg says "the other side of that
movement is BuyerExternal". Nothing precedes it, nothing needs to.

**What `BuyerExternal` represents.** It has `accountType = External` — a
credit-normal clearing bucket that the platform uses to mean **"the outside
world on the rider's side of the fence"**. From the platform's books, the
rider, their bank, and the PG are all lumped into one opaque "outside"
account. A textbook ledger would use a real counterparty account (e.g.
"Rider Receivable"), but the platform does not track individual riders as
ledger counterparties, so it uses a shared `External` bucket.

**Net effect across the two steps** — proving the flow is valid:

| Account          | Type      | Step A | Step B | Net  |
| ---------------- | --------- | ------ | ------ | ---- |
| `BuyerAsset`     | Asset     | +100   | 0      | +100 |
| `BuyerExternal`  | External  | +100   | -100   | 0    |
| `OwnerLiability` | Liability | 0      | +100   | +100 |

`BuyerExternal` is a **zero-sum pass-through** — at the end of the two steps
it returns to where it started. The net effect is identical to writing a
single entry `transfer BuyerAsset OwnerLiability 100`, and the accounting
equation is preserved on both sides: Assets +100 = Liabilities +100.

**So why bother with the intermediate hop at all?** Three reasons:

1. **Audit trail.** Two separate ledger rows means reporting can query
   "cash that entered the system" (Step A) independently from "cash that
   was assigned to the driver wallet" (Step B). One row per business
   meaning makes downstream GL extraction and reconciliation trivial.
2. **Shape-ready for timing separation.** If the team later wants to
   recognise cash receipt at PG webhook time and allocate it to the driver
   only at ride-end time, the structure already exists — you just move
   Step A earlier in the lifecycle. No schema change required.
3. **Clean reversals.** If the PG refunds the rider, Step A and Step B can
   be reversed independently via `createReversal`, each with its own
   reason. A collapsed single-entry version would force an all-or-nothing
   reversal.

**Why the ledger is "lazy".** Neither Step A nor Step B is written when the
rider actually pays — they are both written when the **ride ends**. The
nammayatri ledger is a **settlement record**, not a real-time money-movement
feed. The real-time side of things lives in Juspay's books; the platform
trusts the PG and only commits to its own ledger at business events it
cares about (ride end, cancellation, payout). A more rigorous bank-grade
ledger would write an entry at every micro-event:

```
1. Rider pays via PG        → DR PGClearingAsset / CR RiderLiability
2. PG settles to platform   → DR BankAsset        / CR PGClearingAsset
3. Ride completes           → DR RiderLiability   / CR DriverWalletLiability
```

nammayatri collapses all three into the Step A + Step B pair at ride-end.
It's a deliberate simplification that trades real-time visibility for a
dramatically simpler settlement model — and since the PG is the
authoritative source for steps 1 and 2 anyway, there's no business value
in duplicating them in our ledger.

#### Wallet reflection

Starting balance 0, final driver wallet balance after this ride:

```
+100 BaseRide
+ 15 TollCharges
+ 10 ParkingCharges
-  1 TDS
- 10 Commission
-----
 114  (stored on the driver's Liability account)
```

The entries are written with `status = SETTLED` and
`settlementStatus = UNSETTLED` (meaning: the ledger is finalised, but the
driver has not been disbursed yet).

#### Reasoning per movement

- **`BuyerAsset` grew by ₹130** (100 base + 5 GST + 15 toll + 10 parking):
  that is the cash the merchant received from the payment gateway. Asset
  rises when cash comes in.
- **`OwnerLiability` grew by ₹125** (everything except GST): the platform now
  owes the driver ₹125 for fare + toll + parking reimbursement. Liability
  rises because we acknowledge the debt.
- **`GovtIndirect` grew by ₹5**: GST collected from the rider is owed to the
  tax authority, not to the platform. Parking it in its own liability account
  makes the GSTR-1 filing trivial.
- **Driver wallet then shrank by ₹11** (TDS + commission): TDS is withheld
  on behalf of the government (moves to `GovtDirect`), commission is
  recognised as platform income (moves to `SellerRevenue`). Both are
  liability-to-liability / liability-to-revenue transfers so the balance
  sheet still balances.

#### Cash rides

For a cash ride the rider paid the driver directly, so the `BuyerAsset` leg is
skipped. Only the deductions run:

```haskell
transfer OwnerLiability GovtIndirect taxAmount  "GSTCash"
transfer OwnerLiability GovtDirect   tdsAmount  "TDSDeductionCash"
transfer OwnerLiability SellerRevenue commissionAmount "Commission"
```

Economically the driver has already collected the gross fare in cash from the
rider, so the wallet **shrinks** by the amount the platform needs to net-out
(GST + TDS + commission). That negative wallet balance then gets collected
from the driver via subscription / top-up.

### 5.3 Cancellation (rider cancels after the allowed window)

Entry point:
`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/CancelRide/Internal.hs:256`
→ `cancelRideTransaction`, gated on
`driverWalletConfig.enableDriverWallet` and a non-zero `fee.amount`.

For a ₹50 cancellation fee (GST ₹2, TDS 1%):

```haskell
-- base cancellation: rider cash → driver wallet
transfer_ BuyerAsset     BuyerExternal   48  "CustomerCancellationCharges"
transfer  BuyerExternal  OwnerLiability  48  "CustomerCancellationCharges"

-- GST on the cancellation: rider cash → govt
transfer_ BuyerAsset     BuyerExternal    2  "CustomerCancellationGST"
transfer  BuyerExternal  GovtIndirect     2  "CustomerCancellationGST"

-- TDS withheld from the driver's share
transfer  OwnerLiability GovtDirect       0.48 "TDSDeductionCancellation"

invoice invoiceConfig
```

#### Wallet reflection

Driver wallet: **+48 − 0.48 = +47.52** net credit.
`BuyerAsset`: **+50** (rider paid).
`GovtIndirect`: **+2** (GST payable).
`GovtDirect`: **+0.48** (TDS payable).

#### Reasoning

- The rider was charged for their own late cancellation, so the money flows
  into the platform the same way a completed ride would — `BuyerAsset` first,
  then into the driver wallet and tax buckets.
- There is no commission split on cancellations; the entire base goes to the
  driver (less TDS). This matches the business rule that a cancellation fee
  is compensation for the driver's wasted time, not a billable service.
- **Driver cancellations** follow the same pattern but use
  `DriverCancellationCharges` and usually run in the opposite direction
  (wallet debit) because the _driver_ owes the fee.

### 5.4 Payout (settling the driver's balance out of the platform)

Entry point:
`Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Payout.hs:83`
(Juspay webhook handler).

Two-step commit:

1. **Create the payout ledger entry** via `createWalletEntryDelta` with a
   **negative** amount and reference `WalletPayout`. This debits
   `OwnerLiability` (wallet shrinks) and credits `PGPayoutLiability`
   (the platform now owes Juspay the disbursement).
2. **Mark redeemable entries as `PAID_OUT`** via `markEntriesAsPaidOut`
   (`Backend/lib/finance-kernel/src/Lib/Finance/Ledger/Service.hs:450`),
   stamping them with the `payoutRequestId`. This is purely a bookkeeping
   flag on the original credit entries — the balances are not touched again.

This split lets the driver dashboard say "you earned ₹X last week, ₹Y of it
was paid out on date Z" by joining on `settlementId` without replaying
history.

---

## 6. Quick Mental Model Summary

- **One driver = one `Liability` account**. Its `balance` is the wallet
  number you see in the UI.
- **Money into the wallet** (earnings, toll reimbursement, cancellation fees)
  = **credit** the `OwnerLiability` → balance grows.
- **Money out of the wallet** (GST, TDS, commission, payout) = **debit**
  `OwnerLiability` → balance shrinks.
- **No ledger entries at ride assignment** — the ledger is only written at
  ride end / cancellation / payout.
- **Ledger entries are immutable** — corrections happen via `createReversal`,
  never by updating the row.
- **`settlementStatus`** tracks disbursement (UNSETTLED → PAID_OUT);
  **`status`** tracks logical lifecycle (PENDING/DUE/SETTLED/VOIDED). They
  are orthogonal.
- **`DailyStats`** is for reporting UIs. **Never** derive a balance from it.

## 7. Key Files To Know

| File                                                                                                               | What's in it                                                 |
| ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------ |
| `Backend/lib/finance-kernel/src-read-only/Lib/Finance/Domain/Types/Account.hs`                                     | `AccountType`, `CounterpartyType`, schema                    |
| `Backend/lib/finance-kernel/src-read-only/Lib/Finance/Domain/Types/LedgerEntry.hs`                                 | `LedgerEntry`, `EntryStatus`, `SettlementStatus`             |
| `Backend/lib/finance-kernel/src/Lib/Finance/Ledger/Service.hs`                                                     | `createEntryWithBalanceUpdate`, reversals, settlement        |
| `Backend/lib/finance-kernel/src/Lib/Finance/FinanceM.hs`                                                           | `transfer`, `AccountRole` → account mapping                  |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Finance/Wallet.hs`                    | Wallet helpers, reference-type constants, redeemable balance |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/EndRide/Internal.hs:460`    | `createDriverWalletTransaction` (ride completion)            |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Ride/CancelRide/Internal.hs:256` | `cancelRideTransaction` (ride cancellation)                  |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Payout.hs:83`                    | `juspayPayoutWebhookHandler` (payout)                        |
| `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/DriverWallet.hs`                 | Wallet API (balance, transactions)                           |
