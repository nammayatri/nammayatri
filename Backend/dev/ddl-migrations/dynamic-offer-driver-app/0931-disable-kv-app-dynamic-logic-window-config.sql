-- app_dynamic_logic_window_config says which behaviour rulebook versions judge the
-- driver's last X events (EVENTS) instead of daily/weekly counters. A row is written once,
-- when a version is saved from the dashboard, and must not live only in Redis: if its
-- drain to Postgres failed, the version would act as EVENTS until the Redis copy expired
-- and then silently as TIME. Out of KV, the save writes Postgres directly and fails loudly
-- when that write fails. Written a few times a month, so KV gives it no benefit.
-- Idempotent.
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
    config_value::jsonb,
    '{disableForKV}',
    (config_value::jsonb -> 'disableForKV')
      || (
        SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
        FROM (VALUES ('app_dynamic_logic_window_config')) AS missing(t)
        WHERE NOT (config_value::jsonb -> 'disableForKV') @> to_jsonb(t)
      )
  )::text
WHERE id = 'kv_configs';
