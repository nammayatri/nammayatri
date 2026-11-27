-- The permit tables must read and write the database directly. Driver tables go through
-- the KV cache unless listed in disableForKV, and with the cache on:
--   * duplicate-booking checks and the review queue would miss rows not yet drained;
--   * a row rejected by a permit unique index would stop the drainer.
-- Every environment needs this before the permit APIs are used. Safe to run again.

UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
      config_value::jsonb,
      '{disableForKV}',
      (config_value::jsonb -> 'disableForKV') || (
        SELECT coalesce(jsonb_agg(t.name), '[]'::jsonb)
        FROM unnest(ARRAY[
          'permit_applicant',
          'permit_application',
          'permit_application_document',
          'permit_application_event'
        ]) AS t(name)
        WHERE NOT (config_value::jsonb -> 'disableForKV') ? t.name
      )
    )::text
WHERE id = 'kv_configs';
