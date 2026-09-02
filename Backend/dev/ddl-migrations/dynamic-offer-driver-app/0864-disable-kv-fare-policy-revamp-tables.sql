-- Fare-policy-revamp config tables must NOT run through KV.
--
-- kv_configs has allTablesDisabled=false, so any table absent from
-- disableForKV gets KV writes (Redis + async drainer). These three are
-- low-traffic dashboard config tables whose list queries filter on
-- merchant_operating_city_id — which cannot be a KV secondary key — so reads
-- fall back to Postgres while the row is still only in Redis waiting on the
-- drainer: a dashboard create followed by the UI's immediate refetch reads
-- stale data ("new surge config / alert subscription doesn't show until a
-- hard reload"). Direct-DB reads and writes are immediately consistent, which
-- is what a config table wants. Idempotent.
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
    config_value::jsonb,
    '{disableForKV}',
    (config_value::jsonb -> 'disableForKV')
      || (
        SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
        FROM (VALUES ('surge_config'), ('fare_alert_subscription'), ('fare_policy_change_request')) AS missing(t)
        WHERE NOT (config_value::jsonb -> 'disableForKV') @> to_jsonb(t)
      )
  )::text
WHERE id = 'kv_configs';
