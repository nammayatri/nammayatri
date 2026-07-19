# Cancellation Fee Configuration Guide

## Overview

When a ride is cancelled (by rider or driver), a cancellation fee + tax can be charged to the rider. The fee and tax amounts are configurable via dynamic logic; eligibility (time/wait thresholds) is gated by NammaTags. The system supports both online (card/Stripe) and cash payment modes.

> **Reference migration:** `dev/feature-migrations/0034-cancellation-fee-consolidated.sql` is the single, idempotent example that wires the full feature for one city (supersedes the older `0005`/`0011`/`0031` files). Use it as the template for enabling a new city.

**Division of responsibility (important):**
- **NammaTags gate eligibility** — the time/wait thresholds (driver waited ≥ 3 min for no-show; rider cancelled ≥ 3 min after booking) live *only* in the tag `rule_engine`.
- **Dynamic logic sets amounts only** — no redundant threshold/reason re-checks; the driver-vs-rider amount is selected by `cancelledBy`.

## How It Works

```
Rider/Driver cancels ride
    |
    v
NammaTag validates eligibility (CustomerCancellation / CustomerNoShowCancellation)
    |
    v (if Valid)
Dynamic Logic computes fee + tax (USER-CANCELLATION-DUES)
    |
    v
BPP persists on ride table --> sends to BAP via Beckn quotation breakup
    |                              |
    v                              v
BPP creates finance              BAP parses breakup, persists on ride table
invoice + ledger entries         BAP creates finance invoice + charges Stripe (card) or marks DUE (cash)
```

## Configuration Layers

### Layer 1: Master Switch + BPP Fee Rules (BPP)

```sql
UPDATE atlas_driver_offer_bpp.transporter_config
SET can_add_cancellation_fee = true,  -- false = no cancellation fee at all (single master switch)
    -- Cancellation reason code(s) that qualify a DRIVER cancellation as a penalizable no-show.
    -- Configurable; falls back to CUSTOMER_NO_SHOW when NULL. Keep in sync with the
    -- CustomerNoShowCancellation NammaTag rule_engine (Layer 2), which compares the same reason.
    valid_cancellation_penalty_reasons = '{CUSTOMER_NO_SHOW}',
    -- Payment instruments exempt from the cancellation fee. For an exempt instrument the BPP
    -- computes NO fee and sends none to the BAP (no ledger/invoice entries at all).
    -- Values are PaymentInstrument names, e.g. Cash, UPI, NetBanking.
    cancellation_fee_payment_method_exceptions = '{Cash}'
WHERE merchant_operating_city_id = '<city_id>';
```

### Layer 2: NammaTag Gates (BPP)

NammaTags decide **whether** cancellation charges should apply. Two separate tags control two independent flows:

| Tag Name | Who Cancels | Purpose |
|----------|-------------|---------|
| `CustomerCancellation` | Rider cancels | Gate for rider-initiated cancellation charges |
| `CustomerNoShowCancellation` | Driver cancels with CUSTOMER_NO_SHOW | Gate for driver no-show charges |

Each tag evaluates a rule engine expression and returns `Valid` or `Invalid`. Only `Valid` triggers charge computation.

**Enable rider cancellation charges:**
```sql
INSERT INTO atlas_driver_offer_bpp.namma_tag_v2 (
  category, description, tag_type, merchant_operating_city_id, name, tags, rule_engine, created_at, updated_at
) VALUES (
  'CustomerCancellationValidity',
  'Rider cancellation: charge if driver arrived and waited >= 5 seconds',
  'ApplicationTag',
  '<city_id>',
  'CustomerCancellation',
  '{Valid,Invalid}',
  '{"if":[{"==":[{"var":"cancellationReason.source"},"ByUser"]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"var":"bookingCreatedTime"}]},180]},"Valid","Invalid"]},null]}',
  now(), now()
) ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET rule_engine = EXCLUDED.rule_engine, updated_at = now();

-- Register trigger so tag is evaluated on every RideCancel event
INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
VALUES ('RideCancel', '<city_id>', 'CustomerCancellation', now(), now())
ON CONFLICT (event, merchant_operating_city_id, tag_name) DO NOTHING;
```

**Enable driver no-show charges:**
```sql
INSERT INTO atlas_driver_offer_bpp.namma_tag_v2 (
  category, description, tag_type, merchant_operating_city_id, name, tags, rule_engine, created_at, updated_at
) VALUES (
  'CustomerNoShowCancellationValidity',
  'Driver no-show: charge if driver cancelled with CUSTOMER_NO_SHOW after waiting >= 3 min',
  'ApplicationTag',
  '<city_id>',
  'CustomerNoShowCancellation',
  '{Valid,Invalid}',
  '{"if":[{"and":[{"==":[{"var":"cancellationReason.reasonCode"},"CUSTOMER_NO_SHOW"]},{"==":[{"var":"cancellationReason.source"},"ByDriver"]}]},{"if":[{">=":[{"-":[{"var":"currentTime"},{"if":[{"==":[{"var":"driverArrivalTime"},null]},{"var":"currentTime"},{"var":"driverArrivalTime"}]}]},180]},"Valid","Invalid"]},null]}',
  now(), now()
) ON CONFLICT (merchant_operating_city_id, name) DO UPDATE SET rule_engine = EXCLUDED.rule_engine, updated_at = now();

INSERT INTO atlas_driver_offer_bpp.namma_tag_trigger_v2 (event, merchant_operating_city_id, tag_name, created_at, updated_at)
VALUES ('RideCancel', '<city_id>', 'CustomerNoShowCancellation', now(), now())
ON CONFLICT (event, merchant_operating_city_id, tag_name) DO NOTHING;
```

**To disable:** Delete the tag or its trigger for that city.

### Layer 3: Dynamic Logic — Fee + Tax Amounts (BPP)

The `USER-CANCELLATION-DUES` dynamic logic computes the actual amounts. It returns two fields:

- `cancellationCharges` = base fee (tax-exclusive)
- `cancellationChargesTax` = tax amount (e.g. VAT)

#### Rule Syntax

Each rule uses the `cat` operator to merge a key into the result accumulator:
```json
{"cat":[{"var":""},{"cancellationCharges": <value>}]}
```

**Important:** Each `cat` rule can only set **ONE key**. To set both `cancellationCharges` and `cancellationChargesTax`, use **separate orders** (separate rows in `app_dynamic_logic_element`).

#### Step 1: Create the logic rules

```sql
-- Order 0: Set base cancellation charge
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
  description, domain, logic, "order", merchant_id, version
) VALUES (
  'Base cancellation fee: 3 EUR',
  'USER-CANCELLATION-DUES',
  '{"cat":[{"var":""},{"cancellationCharges":3}]}',
  0, '<merchant_id>', 1
) ON CONFLICT (domain, "order", version) DO UPDATE SET
  logic = EXCLUDED.logic, description = EXCLUDED.description;

-- Order 1: Set cancellation tax (24% VAT on 3 EUR = 0.72)
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_element (
  description, domain, logic, "order", merchant_id, version
) VALUES (
  'Cancellation tax: 0.72 EUR (24% VAT)',
  'USER-CANCELLATION-DUES',
  '{"cat":[{"var":""},{"cancellationChargesTax":0.72}]}',
  1, '<merchant_id>', 1
) ON CONFLICT (domain, "order", version) DO UPDATE SET
  logic = EXCLUDED.logic, description = EXCLUDED.description;
```

#### Step 2: Rollout the logic

```sql
INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout (
  domain, percentage_rollout, version, version_description,
  time_bounds, merchant_operating_city_id, created_at, updated_at
) VALUES (
  'USER-CANCELLATION-DUES', 100, 1, 'Cancellation charges v1',
  'Unbounded', '<city_id>', now(), now()
) ON CONFLICT (domain, merchant_operating_city_id, time_bounds, version) DO UPDATE SET
  percentage_rollout = EXCLUDED.percentage_rollout, updated_at = now();
```

#### Available Input Variables

The dynamic logic receives these variables for writing conditional rules:

| Variable | Type | Description |
|----------|------|-------------|
| `cancelledBy` | String | `CancellationByDriver` or `CancellationByCustomer` |
| `cancellationReasonSelected` | String | e.g. `CUSTOMER_NO_SHOW`, `OTHER` |
| `isArrivedAtPickup` | Bool | Whether driver reached pickup |
| `driverWaitingTime` | Int | Seconds driver waited at pickup |
| `serviceTier` | String | `COMFY`, `AUTO_RICKSHAW`, etc. |
| `estimatedBookingFare` | Number | Estimated fare |
| `totalBookings` | Int | Rider's total bookings |
| `cancelledRides` | Int | Rider's cancelled ride count |
| `completedRides` | Int | Rider's completed rides |
| `cancellationDues` | Number | Rider's pending cancellation dues |
| `tripCategory` | Object | Trip type info |
| `callAttemptByDriver` | Bool | Whether driver called rider |

#### Example: Conditional Overrides

Orders execute as a **pipeline** — each order can read and override values set by previous orders. Use `{"var":"cancellationCharges"}` to read the current value.

**Order 0 — Set base charge (3 EUR):**
```json
{"cat":[{"var":""},{"cancellationCharges":3}]}
```

**Order 1 — Set base tax (0.72 EUR):**
```json
{"cat":[{"var":""},{"cancellationChargesTax":0.72}]}
```

Because the NammaTags already gate eligibility (reason, source, wait time), the dynamic logic does **not** re-check those conditions — it only picks the amount by `cancelledBy`. Order 0/1 set the rider-cancellation amount as the default; orders 2/3 override it for driver no-show.

**Order 2 — Override charge for driver no-show (100 EUR):**
```json
{"cat":[{"var":""},{"cancellationCharges":
  {"if":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},100,{"var":"cancellationCharges"}]}
}]}
```

**Order 3 — Override tax for driver no-show (24 EUR):**
```json
{"cat":[{"var":""},{"cancellationChargesTax":
  {"if":[{"==":[{"var":"cancelledBy"},"CancellationByDriver"]},24,{"var":"cancellationChargesTax"}]}
}]}
```

### Layer 4: BAP Settlement Config

Controls how cancellation fees are collected from the rider:

```sql
-- Immediate capture vs pending due, configured separately for driver- and rider-initiated fees.
--   true  = capture the fee now (Stripe charge for card / mark DUE for cash)
--   false = defer as a pending due (settled later / before the next ride)
-- Defaults to true when unset (preserves prior behaviour).
UPDATE atlas_app.rider_config
SET immediate_capture_driver_cancellation_fee = true,   -- driver no-show fees
    immediate_capture_rider_cancellation_fee = true      -- rider-cancellation fees
WHERE merchant_operating_city_id = '<bap_city_id>';

-- Block rider's next booking until cancellation dues are paid (enables the active settlement path)
UPDATE atlas_app.rider_config
SET settle_cancellation_fee_before_next_ride = true
WHERE merchant_operating_city_id = '<bap_city_id>';
```

> **Deprecated:** `valid_cancellation_reason_codes_for_immediate_charge` is no longer read.
> The immediate-vs-due decision now comes from the two `immediate_capture_*` flags above,
> chosen by cancellation source (`ByUser` → rider flag, otherwise → driver flag).

### Layer 5: BPP Wallet + Invoice Config (for BPP-side finance entries)

Required for BPP-side finance invoice and ledger entries (driver wallet credit):

```sql
-- Enable wallet on merchant level
UPDATE atlas_driver_offer_bpp.merchant
SET prepaid_subscription_and_wallet_enabled = true
WHERE id = '<merchant_id>';

-- Enable driver wallet + invoice config
UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = '{"enableDriverWallet":true,"enableWalletPayout":true,"enableWalletTopup":false,"driverWalletPayoutThreshold":0,"forceOnlineLedger":true,"gstPercentage":0,"minimumWalletPayoutAmount":0,"payoutCutOffDays":0,"maxWalletPayoutsPerDay":null,"minWalletAmountForCashRides":null,"payoutFee":null}',
    -- true = single "Cancellation Fee (Incl. VAT)" line; false = separate "Fee" + "GST" lines
    invoice_config = '{"driverInvoiceLineItemsVatInclusive":true,"emitLedgerEntries":true}'
WHERE merchant_operating_city_id = '<city_id>';
```

## Quick Reference: Enable for a New City

Follow `0034-cancellation-fee-consolidated.sql` as the template. The pieces:

1. **BPP transporter_config**: `can_add_cancellation_fee = true`, `valid_cancellation_penalty_reasons`, `cancellation_fee_payment_method_exceptions`, `driver_wallet_config`, `invoice_config`, `tax_config`
2. **BPP merchant**: `prepaid_subscription_and_wallet_enabled = true`
3. **BPP namma_tag_v2**: Add `CustomerCancellation` and/or `CustomerNoShowCancellation` tags + `RideCancel` triggers (thresholds live here)
4. **BPP app_dynamic_logic_element**: base amount (order 0/1) + driver no-show override (order 2/3), amounts only
5. **BPP app_dynamic_logic_rollout**: Rollout at 100%
6. **BAP rider_config**: `immediate_capture_driver_cancellation_fee`, `immediate_capture_rider_cancellation_fee`, `settle_cancellation_fee_before_next_ride`, `invoice_config`

## Disable for a City

| What to disable | How |
|-----------------|-----|
| All cancellation fees | `can_add_cancellation_fee = false` |
| Fee for a payment method (e.g. cash) | Add the instrument to `cancellation_fee_payment_method_exceptions` (e.g. `{Cash}`) |
| Only rider-cancel fees | Delete `CustomerCancellation` tag or its trigger |
| Only driver no-show fees | Delete `CustomerNoShowCancellation` tag or its trigger |
| Immediate driver no-show capture | `immediate_capture_driver_cancellation_fee = false` (fee becomes a pending due) |
| Immediate rider-cancel capture | `immediate_capture_rider_cancellation_fee = false` (fee becomes a pending due) |
| Next-ride blocking | `settle_cancellation_fee_before_next_ride = false` |
| BPP finance entries | `driver_wallet_config.enableDriverWallet = false` |
| Change the penalty reason | Set `valid_cancellation_penalty_reasons` + the matching `CustomerNoShowCancellation` tag rule |

## Where Data Lives

### Ride Table

| Column | BPP (`atlas_driver_offer_bpp.ride`) | BAP (`atlas_app.ride`) |
|--------|-------------------------------------|------------------------|
| `cancellation_fee` | Base fee (tax-exclusive) | -- |
| `cancellation_fee_tax` | Tax amount | Tax amount |
| `cancellation_charges_on_cancel` | Total (fee + tax) | Total (fee + tax) |

### Finance Tables (both schemas)

| Table | Contents |
|-------|----------|
| `finance_invoice` | Invoice with line items (fee + tax as separate lines) |
| `finance_ledger_entry` | Double-entry ledger: BPP credits driver wallet, BAP debits rider |

### BPP Invoice Line Items

With `driverInvoiceLineItemsVatInclusive = true`:
- "Cancellation Fee (Incl. VAT)" = fee + tax combined

With `driverInvoiceLineItemsVatInclusive = false`:
- "Customer Cancellation Fee" = base fee
- "GST on Cancellation Fee" = tax

### BAP Invoice Line Items
- "Cancellation Fee" = base fee (tax-exclusive)
- "Cancellation Fee VAT" = tax amount

## Settlement Flow (BAP)

| Payment Method | Behavior |
|----------------|----------|
| **Card** | Immediate Stripe charge if reason is in `valid_cancellation_reason_codes_for_immediate_charge`. Pending ledger created, then settled via `makePaymentIntent` + `chargePaymentIntent`. |
| **Cash** | Cannot auto-charge. Ledger marked as DUE. Rider blocked from next booking if `settle_cancellation_fee_before_next_ride = true`. |
