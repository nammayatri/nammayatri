-- kaal_chakra_job_run is a run-history/audit table for the Kaal Chakra Jobs
-- tab. Its dashboard list query filters on merchant_operating_city_id,
-- chakra and status together — none of which can be a KV secondary key
-- (merchant_operating_city_id/status are deny-listed outright) — so it must
-- read straight from Postgres rather than through the KV connector.
-- Idempotent.
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
    config_value::jsonb,
    '{disableForKV}',
    (config_value::jsonb -> 'disableForKV')
      || (
        SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
        FROM (VALUES ('kaal_chakra_job_run')) AS missing(t)
        WHERE NOT (config_value::jsonb -> 'disableForKV') @> to_jsonb(t)
      )
  )::text
WHERE id = 'kv_configs';
