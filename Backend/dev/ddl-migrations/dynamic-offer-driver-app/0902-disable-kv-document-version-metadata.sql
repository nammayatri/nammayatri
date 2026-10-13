-- Low-traffic onboarding table, kept out of KV like digilocker_verification (0732) so its rows
-- are written to Postgres directly. Idempotent.
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
    config_value::jsonb,
    '{disableForKV}',
    (config_value::jsonb -> 'disableForKV')
      || (
        SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
        FROM (VALUES ('document_version_metadata')) AS missing(t)
        WHERE NOT (config_value::jsonb -> 'disableForKV') @> to_jsonb(t)
      )
  )::text
WHERE id = 'kv_configs';
