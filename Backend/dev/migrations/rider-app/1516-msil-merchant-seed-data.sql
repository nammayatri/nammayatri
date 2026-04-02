-- WARNING: LOCAL DEVELOPMENT ONLY — DO NOT RUN IN PRODUCTION
-- =============================================================================
-- MSIL merchant seed data (Rider / atlas_app) for fleet onboarding testing
-- Clones config from base YATRI/NAMMA_YATRI merchant
-- =============================================================================

CREATE OR REPLACE FUNCTION pg_temp.clone_rows(
  p_table TEXT,
  p_where_col TEXT, p_where_val TEXT,
  p_replacements JSONB
) RETURNS void AS $fn$
DECLARE
  cols TEXT[];
  select_parts TEXT[];
  col TEXT;
BEGIN
  SELECT array_agg(column_name::text ORDER BY ordinal_position)
  INTO cols
  FROM information_schema.columns
  WHERE table_schema || '.' || table_name = p_table;

  FOR i IN 1..array_length(cols, 1) LOOP
    col := cols[i];
    IF p_replacements ? col THEN
      select_parts := array_append(select_parts,
        format('%L::%s', p_replacements->>col,
          (SELECT data_type FROM information_schema.columns
           WHERE table_schema || '.' || table_name = p_table AND column_name = col)));
    ELSE
      select_parts := array_append(select_parts, quote_ident(col));
    END IF;
  END LOOP;

  EXECUTE format(
    'INSERT INTO %s SELECT %s FROM %s WHERE %I = %L ON CONFLICT DO NOTHING',
    p_table,
    array_to_string(select_parts, ', '),
    p_table,
    p_where_col,
    p_where_val
  );
END;
$fn$ LANGUAGE plpgsql;

DO $$
DECLARE
  v_rider_base_merchant_id TEXT;
  v_rider_base_city_id TEXT;

  v_msil_rider_merchant_id TEXT := 'msil0000-0000-0000-0000-000000000bap';
  v_msil_rider_city_id TEXT := 'msil0000-0000-0000-0000-0msilbapcity';

BEGIN
  IF current_database() NOT IN ('atlas_dev', 'atlas_dev_test', 'nammayatri_dev') THEN
    RAISE NOTICE 'Skipping MSIL rider seed: not a dev DB.';
    RETURN;
  END IF;

  SELECT id INTO v_rider_base_merchant_id FROM atlas_app.merchant WHERE short_id IN ('YATRI', 'NAMMA_YATRI') LIMIT 1;
  SELECT id INTO v_rider_base_city_id FROM atlas_app.merchant_operating_city WHERE merchant_id = v_rider_base_merchant_id LIMIT 1;

  IF v_rider_base_merchant_id IS NULL THEN
    RAISE NOTICE 'No YATRI rider merchant found — skipping MSIL rider seed.';
    RETURN;
  END IF;

  IF EXISTS (SELECT 1 FROM atlas_app.merchant WHERE short_id = 'MSIL') THEN
    RAISE NOTICE 'MSIL rider merchant already exists — skipping.';
    RETURN;
  END IF;

  RAISE NOTICE 'Creating MSIL rider merchant from base: %', v_rider_base_merchant_id;

  PERFORM pg_temp.clone_rows('atlas_app.merchant', 'id', v_rider_base_merchant_id, jsonb_build_object(
    'id', v_msil_rider_merchant_id,
    'name', 'MSIL',
    'short_id', 'MSIL',
    'city', 'Delhi',
    'state', 'NationalCapitalTerritory',
    'country', 'India'
  ));

  INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city, state, country,
    lat, long, distance_unit)
  VALUES (v_msil_rider_city_id, v_msil_rider_merchant_id, 'MSIL', 'Delhi', 'NationalCapitalTerritory', 'India',
    28.6139, 77.2090, 'Meter')
  ON CONFLICT (id) DO NOTHING;

  PERFORM pg_temp.clone_rows('atlas_app.merchant_service_usage_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_rider_city_id,
    'merchant_id', v_msil_rider_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_app.merchant_service_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_rider_city_id,
    'merchant_id', v_msil_rider_merchant_id
  ));

  RAISE NOTICE 'MSIL rider merchant + city created.';

END;
$$;
