-- fare_adjustment must NOT run through KV, for the same reason as the tables
-- in 0864-disable-kv-fare-policy-revamp-tables.sql.
--
-- It has exactly that shape: the dashboard lists adjustments with
-- findAllByMerchantOperatingCityId (a KV secondary key, so the read falls back
-- to Postgres) while postPricingAdjustmentStatus writes the new status with
-- updateOneWithKV (Redis, drained to Postgres asynchronously). Between the two
-- the row is only in Redis, so the list keeps reporting the old status.
--
-- Seen on production: a temporary boost was ended successfully, the list and
-- the Studio went on showing it ACTIVE, and retrying returned
-- "transition ENDED -> ENDED is not allowed" — the write path saw ENDED while
-- every reader still saw ACTIVE. An operator cannot tell whether a boost is
-- live, which is the one thing this screen exists to answer.
--
-- Direct-DB reads and writes are immediately consistent, which is what a
-- low-traffic dashboard config table wants. Idempotent.
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
    config_value::jsonb,
    '{disableForKV}',
    (config_value::jsonb -> 'disableForKV')
      || (
        SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
        FROM (VALUES ('fare_adjustment')) AS missing(t)
        WHERE NOT (config_value::jsonb -> 'disableForKV') @> to_jsonb(t)
      )
  )::text
WHERE id = 'kv_configs';
