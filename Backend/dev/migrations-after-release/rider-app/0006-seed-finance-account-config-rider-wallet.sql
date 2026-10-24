-- Finance-kernel unification, rider side: enable kernel transfers for the rider
-- wallet class (Liability, RIDER). One merchant-wide ('*' city) row per merchant;
-- without it the class defaults are transfer_out_enabled = false and every cashback /
-- referral / metro payout falls back to the legacy (unledgered-hold) path.
--
-- allow_negative_balance stays TRUE (kernel default behaviour) until the deposit
-- flows are audited for a strict class.

INSERT INTO atlas_app.finance_account_config
  (id, merchant_id, merchant_operating_city_id, account_type, counterparty_type, sub_ledger,
   allow_negative_balance, min_retained_balance, redeemable_cutoff,
   transfer_out_enabled, instant_transfer_out_enabled, min_transfer_out_amount,
   max_transfer_out_per_day, transfer_out_fee, transfer_in_enabled, auto_transfer_out,
   created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || m.id),
  m.id,
  '*',
  'Liability',
  'RIDER',
  NULL,
  true,
  0,
  NULL, -- cashback has no redeemable cutoff: everything unsettled is payable
  true,
  true,
  0,
  NULL,
  NULL,
  true, -- booking-deposit topups are inbound transfers
  NULL,
  now(),
  now()
FROM atlas_app.merchant m
WHERE NOT EXISTS (
  SELECT 1
  FROM atlas_app.finance_account_config fac
  WHERE fac.merchant_id = m.id
    AND fac.account_type = 'Liability'
    AND fac.counterparty_type = 'RIDER'
    AND fac.sub_ledger IS NULL
);

-- Refund poll pacing moved from rider_config.refund_status_update_interval/_retries
-- (fields DELETED from the Haskell type; DB columns remain, rollback-safe) onto the
-- (Liability, RIDER) class config. Cities whose rider_config carried NON-NULL values
-- get a per-city row (per-city beats the merchant-wide '*' row in the resolution
-- ladder); NULL values meant the old 43200s/5 defaults, which the code now hard-codes,
-- so those cities need no row. Idempotent: per-city insert guarded by NOT EXISTS, and
-- a COALESCE backfill for pre-existing per-city rows — never clobbers tuned values.

INSERT INTO atlas_app.finance_account_config
  (id, merchant_id, merchant_operating_city_id, account_type, counterparty_type, sub_ledger,
   allow_negative_balance, min_retained_balance, redeemable_cutoff,
   transfer_out_enabled, instant_transfer_out_enabled, min_transfer_out_amount,
   max_transfer_out_per_day, transfer_out_fee, transfer_in_enabled, auto_transfer_out,
   status_check_interval_sec, status_check_max_attempts,
   created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || rc.merchant_operating_city_id),
  rc.merchant_id,
  rc.merchant_operating_city_id,
  'Liability',
  'RIDER',
  NULL,
  true,
  0,
  NULL,
  true,
  true,
  0,
  NULL,
  NULL,
  true,
  NULL,
  rc.refund_status_update_interval,
  rc.refund_status_update_retries,
  now(),
  now()
FROM atlas_app.rider_config rc
WHERE rc.merchant_id IS NOT NULL
  AND (rc.refund_status_update_interval IS NOT NULL OR rc.refund_status_update_retries IS NOT NULL)
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_app.finance_account_config fac
    WHERE fac.merchant_operating_city_id = rc.merchant_operating_city_id
      AND fac.account_type = 'Liability'
      AND fac.counterparty_type = 'RIDER'
      AND fac.sub_ledger IS NULL
  );

UPDATE atlas_app.finance_account_config fac
SET status_check_interval_sec = COALESCE(fac.status_check_interval_sec, rc.refund_status_update_interval),
    status_check_max_attempts = COALESCE(fac.status_check_max_attempts, rc.refund_status_update_retries),
    updated_at = now()
FROM atlas_app.rider_config rc
WHERE fac.merchant_operating_city_id = rc.merchant_operating_city_id
  AND fac.account_type = 'Liability'
  AND fac.counterparty_type = 'RIDER'
  AND fac.sub_ledger IS NULL
  AND (rc.refund_status_update_interval IS NOT NULL OR rc.refund_status_update_retries IS NOT NULL)
  AND (fac.status_check_interval_sec IS NULL OR fac.status_check_max_attempts IS NULL);
