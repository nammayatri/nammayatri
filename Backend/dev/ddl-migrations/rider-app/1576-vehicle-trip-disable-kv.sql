-- vehicle_trip must write straight to Postgres: its partial unique index and CHECKs
-- only guard synchronous writes, and a KV write would reach them only at drain time.
-- Prod/master kv_configs need the same entry.
UPDATE atlas_app.system_configs
SET config_value = (
    jsonb_set(
        config_value::jsonb,
        '{disableForKV}',
        (config_value::jsonb -> 'disableForKV') || '"vehicle_trip"'
    )
)::text
WHERE id = 'kv_configs'
  AND NOT (config_value::jsonb -> 'disableForKV') ? 'vehicle_trip';
