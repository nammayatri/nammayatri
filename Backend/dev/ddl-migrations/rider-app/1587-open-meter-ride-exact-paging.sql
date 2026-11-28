-- open_meter_ride's trip history pages by offset (GET /v2/openMeter/rides?limit&offset). Outside
-- tablesForExactPaging the KV connector merges every KV-resident row of the rider into each page and
-- lowers the Postgres offset by the number of KV rows, although the drainer has already copied those
-- rows to Postgres. A page after the first then repeats rows of earlier pages and skips others.
-- Exact paging leaves the KV rows out of the Postgres query by primary key and pages the merge.
--
-- Rollout: apply to each environment's atlas_app.system_configs before the app pages trip history by
-- offset. Riders see duplicate and missing trips past the first page until then. rider-app reloads
-- kv_configs on its own refresh interval, so no restart is needed. Idempotent. A tablesForExactPaging
-- that is not an array (rider-app could not decode it) is replaced by one. Without a kv_configs row
-- this changes nothing, and nothing needs it: the kernel's default KV config (allTablesDisabled)
-- keeps every table out of KV, so trip history reads Postgres alone and pages exactly.
UPDATE atlas_app.system_configs AS sc
SET config_value = jsonb_set(
    sc.config_value::jsonb,
    '{tablesForExactPaging}',
    CASE
      WHEN normalized.tables @> '["open_meter_ride"]'::jsonb THEN normalized.tables
      ELSE normalized.tables || '["open_meter_ride"]'::jsonb
    END
  )::text
FROM (
  SELECT id,
         CASE
           WHEN jsonb_typeof(config_value::jsonb -> 'tablesForExactPaging') = 'array'
             THEN config_value::jsonb -> 'tablesForExactPaging'
           ELSE '[]'::jsonb
         END AS tables
  FROM atlas_app.system_configs
  WHERE id = 'kv_configs'
) AS normalized
WHERE sc.id = normalized.id;
