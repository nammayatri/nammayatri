-- WARNING: LOCAL DEVELOPMENT ONLY — DO NOT RUN IN PRODUCTION
-- =============================================================================
-- MSIL_PARTNER merchant seed data for fleet onboarding testing
-- Clones config from base NAMMA_YATRI merchant
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
  v_driver_base_merchant_id TEXT;
  v_driver_base_city_id TEXT;

  -- MSIL_PARTNER IDs
  v_msil_driver_merchant_id TEXT := 'msil0000-0000-0000-0000-00000000msil';
  v_msil_driver_city_id TEXT := 'msil0000-0000-0000-0000-0000msilcity';

BEGIN
  -- Safety check
  IF current_database() NOT IN ('atlas_dev', 'atlas_dev_test', 'nammayatri_dev') THEN
    RAISE NOTICE 'Skipping MSIL seed: database "%" does not look like a local dev DB.', current_database();
    RETURN;
  END IF;

  SELECT id INTO v_driver_base_merchant_id FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'NAMMA_YATRI_PARTNER' LIMIT 1;
  SELECT id INTO v_driver_base_city_id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_id = v_driver_base_merchant_id LIMIT 1;

  IF v_driver_base_merchant_id IS NULL THEN
    RAISE NOTICE 'No NAMMA_YATRI_PARTNER merchant found — skipping MSIL seed.';
    RETURN;
  END IF;

  -- Skip if MSIL already exists
  IF EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.merchant WHERE short_id = 'MSIL_PARTNER') THEN
    RAISE NOTICE 'MSIL_PARTNER already exists — skipping.';
    RETURN;
  END IF;

  RAISE NOTICE 'Creating MSIL_PARTNER from base merchant: %', v_driver_base_merchant_id;

  -- ===================== MSIL_PARTNER (Driver BPP) =====================

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant', 'id', v_driver_base_merchant_id, jsonb_build_object(
    'id', v_msil_driver_merchant_id,
    'name', 'MSIL Partner',
    'subscriber_id', 'MSIL_PARTNER',
    'short_id', 'MSIL_PARTNER',
    'city', 'Delhi',
    'state', 'NationalCapitalTerritory',
    'country', 'India',
    'unique_key_id', 'juspay-mobility-bpp-1-key',
    'online_payment', 'true'
  ));

  INSERT INTO atlas_driver_offer_bpp.merchant_operating_city (id, merchant_id, merchant_short_id, city, state, country,
    lat, lon, distance_unit, currency, language)
  VALUES (v_msil_driver_city_id, v_msil_driver_merchant_id, 'MSIL_PARTNER', 'Delhi', 'NationalCapitalTerritory', 'India',
    28.6139, 77.2090, 'Meter', 'INR', 'ENGLISH')
  ON CONFLICT (id) DO NOTHING;

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.transporter_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_driver_city_id,
    'merchant_id', v_msil_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.driver_intelligent_pool_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_driver_city_id,
    'merchant_id', v_msil_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.driver_pool_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_driver_city_id,
    'merchant_id', v_msil_driver_merchant_id,
    'id', md5(v_msil_driver_city_id || 'pool')
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant_service_usage_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_driver_city_id,
    'merchant_id', v_msil_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant_service_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_msil_driver_city_id,
    'merchant_id', v_msil_driver_merchant_id
  ));

  -- vehicle_service_tier
  INSERT INTO atlas_driver_offer_bpp.vehicle_service_tier (id, air_conditioned, allowed_vehicle_variant, driver_rating, long_description, merchant_id, merchant_operating_city_id, name, seating_capacity, short_description, vehicle_rating, service_tier_type, default_for_vehicle_variant, priority, auto_selected_vehicle_variant, ventilator, oxygen, is_air_conditioned)
  SELECT md5(random()::text || vst.id), vst.air_conditioned, vst.allowed_vehicle_variant, vst.driver_rating, vst.long_description,
    v_msil_driver_merchant_id, v_msil_driver_city_id,
    vst.name, vst.seating_capacity, vst.short_description, vst.vehicle_rating, vst.service_tier_type, vst.default_for_vehicle_variant, vst.priority, vst.auto_selected_vehicle_variant, vst.ventilator, vst.oxygen, vst.is_air_conditioned
  FROM atlas_driver_offer_bpp.vehicle_service_tier vst
  WHERE vst.merchant_operating_city_id = v_driver_base_city_id
  ON CONFLICT DO NOTHING;

  -- fare_product
  INSERT INTO atlas_driver_offer_bpp.fare_product (id, area, disable_recompute, enabled, fare_policy_id, merchant_id, merchant_operating_city_id, search_source, time_bounds, trip_category, vehicle_variant)
  SELECT md5(random()::text || fp.id), fp.area, fp.disable_recompute, fp.enabled, fp.fare_policy_id,
    v_msil_driver_merchant_id, v_msil_driver_city_id,
    fp.search_source, fp.time_bounds, fp.trip_category, fp.vehicle_variant
  FROM atlas_driver_offer_bpp.fare_product fp
  WHERE fp.merchant_operating_city_id = v_driver_base_city_id
    AND fp.enabled = true AND fp.area = 'Default' AND fp.time_bounds = 'Unbounded'
  ON CONFLICT DO NOTHING;

  -- Document verification config (clone from base — needed for PAN/Aadhaar/GST verification)
  INSERT INTO atlas_driver_offer_bpp.document_verification_config
    (check_expiry, check_extraction, dependency_document_type, description, disable_warning,
     document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
     merchant_id, merchant_operating_city_id, rc_number_prefix_list, supported_vehicle_classes_json,
     title, vehicle_category, vehicle_class_check_type, created_at, updated_at,
     "order", is_default_enabled_on_manual_verification, is_image_validation_required,
     do_strict_verifcation, filter_for_old_apks, role, document_category, is_mandatory_for_enabling, document_fields_json)
  SELECT check_expiry, check_extraction, dependency_document_type, description, disable_warning,
     document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
     v_msil_driver_merchant_id, v_msil_driver_city_id, rc_number_prefix_list, supported_vehicle_classes_json,
     title, vehicle_category, vehicle_class_check_type, now(), now(),
     "order", is_default_enabled_on_manual_verification, is_image_validation_required,
     do_strict_verifcation, filter_for_old_apks, role, document_category, is_mandatory_for_enabling, document_fields_json
  FROM atlas_driver_offer_bpp.document_verification_config
  WHERE merchant_operating_city_id = v_driver_base_city_id
  ON CONFLICT DO NOTHING;

  -- Fleet owner document verification config (clone from base)
  INSERT INTO atlas_driver_offer_bpp.fleet_owner_document_verification_config
    (check_expiry, check_extraction, dependency_document_type, description, disable_warning,
     document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
     merchant_id, merchant_operating_city_id, "order", title,
     created_at, updated_at, is_image_validation_required, is_default_enabled_on_manual_verification,
     do_strict_verifcation, role, document_category)
  SELECT check_expiry, check_extraction, dependency_document_type, description, disable_warning,
     document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
     v_msil_driver_merchant_id, v_msil_driver_city_id, "order", title,
     now(), now(), is_image_validation_required, is_default_enabled_on_manual_verification,
     do_strict_verifcation, role, document_category
  FROM atlas_driver_offer_bpp.fleet_owner_document_verification_config
  WHERE merchant_operating_city_id = v_driver_base_city_id
  ON CONFLICT DO NOTHING;

  -- NOTE: BPP Dashboard merchant + access matrix moved to provider-dashboard/0077-msil-merchant-seed-data.sql

  -- Set Idfy as verification provider (not GovtData) and remove rate limits for local testing
  UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
  SET verification_providers_priority_list = '{Idfy}'
  WHERE merchant_operating_city_id = v_msil_driver_city_id;

  UPDATE atlas_driver_offer_bpp.transporter_config
  SET onboarding_try_limit = 9999, onboarding_retry_time_in_hours = 0,
      allow_duplicate_aadhaar = false, allow_duplicate_pan = false, allow_duplicate_gst = false
  WHERE merchant_operating_city_id = v_msil_driver_city_id;

  -- Disable strict verification for local testing
  UPDATE atlas_driver_offer_bpp.document_verification_config
  SET do_strict_verifcation = false
  WHERE merchant_operating_city_id = v_msil_driver_city_id;

  RAISE NOTICE 'MSIL_PARTNER merchant + city created.';

END;
$$;
