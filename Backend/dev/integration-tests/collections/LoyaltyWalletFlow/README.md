# LoyaltyWalletFlow

Integration test for the loyalty wallet / Juspay LoyaltyOS flow added in PR #14507.

## What this test exercises

The PR adds two ledger-write paths driven off Juspay's `loyalty_info` block on
the order-status response:

1. **`postWalletRecharge`** (`Domain.Action.UI.Payment`) — creates a `wallet_payments`
   row (status `NEW`, kind `TOPUP`) when `programId` is in the Juspay
   `loyaltyProgramMap` config.
2. **`orderStatusHandlerWithRefunds`** (`SharedLogic.Payment`) — when the
   status response carries `orderLoyaltyInfo`, calls
   `processLoyaltyInfoFromOrderStatus`, which writes `LOYALTY_EARN_*` and
   `LOYALTY_BURN` ledger entries, bumps wallet aggregates, and reconciles
   against Juspay's `/loyalty/programs` endpoint.

## Prerequisites

1. Mock server running with the `loyaltyInfo` handler added in
   `Backend/dev/mock-servers/services/juspay.py` (this PR).
2. Chennai merchant has a `JuspayWallet_Juspay` row in `merchant_service_config`
   with `loyaltyProgramMap` containing
   `019d9617-abeb-7a92-ac58-0d58052508c4 -> LOYALTY_WALLET`.
   Run once to seed:
   ```
   psql -h localhost -p 5434 -U atlas_superuser -d atlas_dev \
     -f Backend/dev/feature-migrations/0008-loyalty-wallet-program-map.sql
   ```

## Suites

| File | Juspay test ID | Scenario |
|------|---------------|----------|
| `01-WalletRechargeTopup.json` | `test-Zumgy8FTRw` | `POST /payment/wallet/recharge` -> Juspay returns CHARGED with TOPUP earn 500 -> wallet_payments row + ledger LOYALTY_EARN_TOPUP |

## Run

```
./run-tests.sh loyalty                                   # all suites
./run-tests.sh loyalty FRFS_Chennai                      # all suites for Chennai
./run-tests.sh loyalty FRFS_Chennai 01-WalletRechargeTopup
```

## Deferred work

- **FRFS-paid (bus / metro / train / multimodal) variants** (`test-ofjHUmkXZ0`,
  `test-hF8CoyIpTR`, `test-kY6NdbJ2Hy`, `test-thgY9qhRKl`, `test-00rbbuqf8i`,
  `test-mJCg8WL9pk`, `test-SZXKuLJr6C`, `test-QeVcWkhSKp`): same loyalty code
  path, just different booking scaffolding. Copy
  `BusTicketBookingFlow/01-DirectBusBooking.json` /
  `MetroTicketBookingFlow/01-DirectMetroBooking.json` /
  `SubwayTicketBookingFlow/01-DirectSubwayBooking.json` and attach the
  loyalty-info mock-status overrides.
- **DB-level assertions**: Newman cannot query Postgres. Currently the suite
  only asserts HTTP responses on auth / recharge / order-status. To verify
  `wallet`/`wallet_payments`/`ledger_entry` rows directly, either
  (a) add a `/debug/wallet` admin endpoint to the rider-app or
  (b) extend `run-tests.sh` with a post-suite `psql` check.
- **Failure / reversal scenarios**: not yet covered. Add suites for
  `AUTO_REFUNDED` (triggers `recordLoyaltyEarnReversal`) and
  `JUSPAY_DECLINED` (no ledger write).
