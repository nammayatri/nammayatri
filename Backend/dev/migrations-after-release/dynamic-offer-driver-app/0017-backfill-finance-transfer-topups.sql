-- Finance-kernel unification: POST-RELEASE backfill of finance_transfer from the
-- transitional wallet_transaction dual-write (and from pre-kernel history).
--
-- Run AFTER the rollout is fully complete (no old pods minting wallet_transaction-only
-- topups). Once this has run and is verified, the dual-write in postWalletTopup /
-- processWalletTopupWebhook and then the wallet_transaction table itself can be dropped.
--
-- Idempotent: the (reference_type, reference_id) unique constraint + ON CONFLICT
-- make re-runs no-ops. Entry linkage comes from the credit legs already stamped with
-- the order id as the ledger reference.

INSERT INTO atlas_driver_offer_bpp.finance_transfer
  (id, direction, status, rail_key, rail_status, rail_order_id, account_id,
   counter_account_id, requested_amount, amount, fee_amount, currency,
   reference_type, reference_id, hold_entry_id, entry_ids, covered_entry_ids,
   metadata, scheduled_for, attempt_count, last_error,
   merchant_id, merchant_operating_city_id, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || wt.id),
  'INBOUND',
  CASE wt.status
    WHEN 'SUCCESS' THEN 'SUCCEEDED'
    WHEN 'FAILED' THEN 'FAILED'
    WHEN 'REFUNDED' THEN 'REVERSED'
    WHEN 'EXPIRED' THEN 'FAILED'
    WHEN 'PROCESSING' THEN 'IN_FLIGHT'
    ELSE 'CREATED'
  END,
  'JUSPAY_PG',
  NULL,
  wt.payment_order_id,
  fa.id,
  NULL,
  wt.amount,
  wt.amount,
  0,
  fa.currency,
  'WalletTopup',
  wt.payment_order_id,
  NULL,
  ( SELECT array_agg(le.id)
    FROM atlas_driver_offer_bpp.finance_ledger_entry le
    WHERE le.reference_type = 'WalletTopup' AND le.reference_id = wt.payment_order_id
  ),
  NULL,
  NULL,
  NULL,
  1,
  CASE
    WHEN wt.status = 'EXPIRED' THEN 'SUPERSEDED_BY_NEW_INTENT'
    WHEN wt.status = 'SUCCESS' THEN NULL -- a settled transfer carries no error
    ELSE 'BACKFILLED_FROM_WALLET_TRANSACTION'
  END,
  fa.merchant_id,
  fa.merchant_operating_city_id,
  wt.created_at,
  wt.updated_at
FROM atlas_driver_offer_bpp.wallet_transaction wt
JOIN atlas_driver_offer_bpp.finance_account fa
  ON fa.counterparty_type = 'DRIVER'
 AND fa.counterparty_id = wt.driver_id
 AND fa.account_type = 'Liability'
 AND fa.sub_ledger IS NULL
 AND (wt.merchant_id IS NULL OR fa.merchant_id = wt.merchant_id)
 -- account identity includes the operating city; without this a driver with pooled
 -- accounts in two cities would attach the row to an arbitrary one
 AND (wt.merchant_operating_city_id IS NULL OR fa.merchant_operating_city_id = wt.merchant_operating_city_id)
ON CONFLICT (reference_id, reference_type) DO NOTHING;

-- OUTBOUND wallet payouts, from payout_request (entity DRIVER_WALLET_TRANSACTION):
-- gives the per-day cap its history (it counts finance_transfer rows) and gives every
-- pre-migration payout a transfer row, so settlement stops relying on webhook-time
-- backfill. beneficiary may be a driver OR a fleet owner — match either class row.
INSERT INTO atlas_driver_offer_bpp.finance_transfer
  (id, direction, status, rail_key, rail_status, rail_order_id, account_id,
   counter_account_id, requested_amount, amount, fee_amount, currency,
   reference_type, reference_id, hold_entry_id, entry_ids, covered_entry_ids,
   metadata, scheduled_for, attempt_count, last_error,
   merchant_id, merchant_operating_city_id, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || pr.id),
  'OUTBOUND',
  CASE pr.status
    WHEN 'CREDITED' THEN 'SUCCEEDED'
    WHEN 'CASH_PAID' THEN 'SUCCEEDED'
    WHEN 'FAILED' THEN 'FAILED'
    WHEN 'AUTO_PAY_FAILED' THEN 'FAILED'
    WHEN 'CANCELLED' THEN 'FAILED'
    WHEN 'EXCLUDED' THEN 'FAILED'
    WHEN 'INITIATED' THEN 'SUBMITTED'
    WHEN 'CASH_PENDING' THEN 'NEEDS_REVIEW'
    ELSE 'IN_FLIGHT' -- PROCESSING / RETRYING
  END,
  'JUSPAY_PAYOUT',
  NULL,
  NULL,
  fa.id,
  NULL,
  COALESCE(pr.amount, 0)::numeric,
  COALESCE(pr.amount, 0)::numeric,
  COALESCE(pr.payout_fee, 0)::numeric,
  fa.currency,
  'WalletPayout',
  pr.id,
  NULL,
  NULL,
  pr.ledger_entry_ids,
  NULL,
  NULL,
  1,
  CASE WHEN pr.status IN ('CREDITED', 'CASH_PAID') THEN NULL ELSE pr.failure_reason END,
  fa.merchant_id,
  fa.merchant_operating_city_id,
  pr.created_at,
  pr.updated_at
FROM atlas_driver_offer_bpp.payout_request pr
JOIN atlas_driver_offer_bpp.finance_account fa
  ON fa.counterparty_id = pr.beneficiary_id
 AND fa.counterparty_type IN ('DRIVER', 'FLEET_OWNER')
 AND fa.account_type = 'Liability'
 AND fa.sub_ledger IS NULL
 AND fa.merchant_id = pr.merchant_id
 AND fa.merchant_operating_city_id = pr.merchant_operating_city_id
WHERE pr.entity_name = 'DRIVER_WALLET_TRANSACTION'
ON CONFLICT (reference_id, reference_type) DO NOTHING;
