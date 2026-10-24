-- Phase 3 of the finance-kernel unification (docs/backend/design/finance-kernel-unification-plan.md §5.3):
-- seed finance_scheduled_transfer_rule from the DRIVER_WALLET_TRANSACTION rows of
-- scheduled_payout_config, and point the (Liability, DRIVER) class config at the rule.
-- ScheduledBatchPayout dual-reads: kernel rule when named+enabled, legacy config otherwise,
-- so this seed is the switch that moves the sweep schedule's source of truth.

INSERT INTO atlas_driver_offer_bpp.finance_scheduled_transfer_rule
  (id, name, merchant_id, merchant_operating_city_id, account_type, counterparty_type, sub_ledger,
   is_enabled, frequency, time_of_day, day_of_week, day_of_month, interval_days, interval_hours,
   time_diff_from_utc, minimum_amount, batch_size, max_retries, reschedule_buffer_minutes,
   default_rail_key, bulk_status_check_interval_minutes, bulk_status_check_batch_limit,
   order_type, remark, created_at, updated_at)
SELECT
  md5(random()::text || clock_timestamp()::text || spc.merchant_operating_city_id),
  'driver-wallet-sweep',
  spc.merchant_id,
  spc.merchant_operating_city_id,
  'Liability',
  'DRIVER',
  NULL,
  spc.is_enabled,
  spc.frequency,
  spc.time_of_day,
  spc.day_of_week,
  spc.day_of_month,
  spc.interval_days,
  spc.interval_hours,
  spc.time_diff_from_utc,
  spc.minimum_payout_amount,
  spc.batch_size,
  spc.max_retries_per_driver,
  spc.reschedule_buffer_minutes,
  NULL, -- default_rail_key: registry-key vocabulary; spc.default_payout_rail is a batch rail (NEFT/...), deliberately NOT carried
  spc.bulk_status_check_interval_minutes,
  spc.bulk_status_check_batch_limit,
  spc.order_type,
  spc.remark,
  now(),
  now()
FROM atlas_driver_offer_bpp.scheduled_payout_config spc
WHERE spc.payout_category = 'DRIVER_WALLET_TRANSACTION'
  AND NOT EXISTS (
    SELECT 1
    FROM atlas_driver_offer_bpp.finance_scheduled_transfer_rule r
    WHERE r.merchant_operating_city_id = spc.merchant_operating_city_id
      AND r.name = 'driver-wallet-sweep'
  );

-- Backfill the bulk-pacing / submission-dressing columns on rules seeded before these
-- columns existed (the NOT EXISTS guard above skips already-seeded cities). Idempotent:
-- only NULL columns are filled, never overwriting an ops-tuned value.
UPDATE atlas_driver_offer_bpp.finance_scheduled_transfer_rule r
SET bulk_status_check_interval_minutes = COALESCE(r.bulk_status_check_interval_minutes, spc.bulk_status_check_interval_minutes),
    bulk_status_check_batch_limit = COALESCE(r.bulk_status_check_batch_limit, spc.bulk_status_check_batch_limit),
    order_type = COALESCE(r.order_type, spc.order_type),
    remark = COALESCE(r.remark, spc.remark),
    updated_at = now()
FROM atlas_driver_offer_bpp.scheduled_payout_config spc
WHERE r.name = 'driver-wallet-sweep'
  AND spc.merchant_operating_city_id = r.merchant_operating_city_id
  AND spc.payout_category = 'DRIVER_WALLET_TRANSACTION'
  AND (r.bulk_status_check_interval_minutes IS NULL
       OR r.bulk_status_check_batch_limit IS NULL
       OR r.order_type IS NULL
       OR r.remark IS NULL);

UPDATE atlas_driver_offer_bpp.finance_account_config fac
SET auto_transfer_out = 'driver-wallet-sweep',
    updated_at = now()
FROM atlas_driver_offer_bpp.finance_scheduled_transfer_rule r
WHERE fac.merchant_operating_city_id = r.merchant_operating_city_id
  AND r.name = 'driver-wallet-sweep'
  AND fac.account_type = 'Liability'
  AND fac.counterparty_type = 'DRIVER'
  AND fac.sub_ledger IS NULL
  AND fac.auto_transfer_out IS NULL;
