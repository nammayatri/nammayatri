-- shared_cab_blame_count must write straight to Postgres: the atomic upsert bump
-- (Storage.Queries.SharedCabBlameCountExtra.bump) is raw SQL, which KV would bypass.
-- Prod/master kv_configs need the same entry.
-- Twin of 1576-vehicle-trip-disable-kv.sql; creates disableForKV if the key is absent.
UPDATE atlas_app.system_configs
SET config_value = (
    jsonb_set(
        config_value::jsonb,
        '{disableForKV}',
        COALESCE(config_value::jsonb -> 'disableForKV', '[]'::jsonb) || '"shared_cab_blame_count"'
    )
)::text
WHERE id = 'kv_configs'
  AND NOT COALESCE(config_value::jsonb -> 'disableForKV', '[]'::jsonb) ? 'shared_cab_blame_count';
