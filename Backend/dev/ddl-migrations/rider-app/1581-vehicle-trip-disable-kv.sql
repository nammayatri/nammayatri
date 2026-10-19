-- vehicle_trip must write straight to Postgres: its partial unique index and CHECKs
-- only guard synchronous writes, and a KV write would reach them only at drain time.
-- Prod/master kv_configs need the same entry. Creates disableForKV if the key is absent.
UPDATE atlas_app.system_configs
SET config_value = (
    jsonb_set(
        config_value::jsonb,
        '{disableForKV}',
        COALESCE(config_value::jsonb -> 'disableForKV', '[]'::jsonb) || '"vehicle_trip"'
    )
)::text
WHERE id = 'kv_configs'
  AND NOT COALESCE(config_value::jsonb -> 'disableForKV', '[]'::jsonb) ? 'vehicle_trip';
