-- open_meter_ride's trip history pages by offset (GET /v2/openMeter/rides?limit&offset). Outside
-- tablesForExactPaging the KV connector merges every KV-resident row of the rider into each page and
-- lowers the Postgres offset by the number of KV rows, although the drainer has already copied those
-- rows to Postgres. A page after the first then repeats rows of earlier pages and skips others.
-- Exact paging leaves the KV rows out of the Postgres query by primary key and pages the merge.
--
-- Rollout: apply to each environment's atlas_app.system_configs before the app pages trip history by
-- offset. Riders see duplicate and missing trips past the first page until then. rider-app reloads
-- kv_configs on its own refresh interval, so no restart is needed. Idempotent.
UPDATE atlas_app.system_configs
SET config_value = jsonb_set(
    config_value::jsonb,
    '{tablesForExactPaging}',
    (CASE
       WHEN jsonb_typeof(config_value::jsonb -> 'tablesForExactPaging') = 'array'
         THEN config_value::jsonb -> 'tablesForExactPaging'
       ELSE '[]'::jsonb
     END)
      || (
        SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
        FROM (VALUES ('open_meter_ride')) AS missing(t)
        WHERE NOT COALESCE(config_value::jsonb -> 'tablesForExactPaging', '[]'::jsonb) @> to_jsonb(t)
      )
  )::text
WHERE id = 'kv_configs';
