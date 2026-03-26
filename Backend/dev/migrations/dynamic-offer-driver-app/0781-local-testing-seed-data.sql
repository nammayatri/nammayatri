-- WARNING: LOCAL DEVELOPMENT ONLY — DO NOT RUN IN PRODUCTION
-- =============================================================================
-- Test Seed Data (Driver / atlas_driver_offer_bpp): Merchants, Cities, Persons, Tokens
--
-- Strategy: Clone all config rows from base NAMMA_YATRI merchant using dynamic SQL
-- to avoid hardcoding column lists (which break when columns are added).
--
-- Creates:
--   BHARAT_TAXI_PARTNER (BPP) — Delhi, INR
--   LYNX_PARTNER (BPP) — Helsinki, EUR
--   Test drivers + registration tokens for each merchant
--
-- Prerequisites: NAMMA_YATRI merchant must already exist (from base seed).
-- =============================================================================

-- Helper: Clone rows from a table, replacing key column values.
-- Copies ALL columns via dynamic SQL so we never miss NOT NULL columns.
CREATE OR REPLACE FUNCTION pg_temp.clone_rows(
  p_table TEXT,
  p_where_col TEXT, p_where_val TEXT,
  p_replacements JSONB  -- {"column_name": "new_value", ...}
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

  -- BHARAT_TAXI_PARTNER IDs
  v_bt_driver_merchant_id TEXT := 'bharat-t-bpp0-0000-0000-000000000000';
  v_bt_driver_city_id TEXT := 'bt-partn-city-delh-0000-000000000000';

  -- LYNX_PARTNER IDs
  v_lynx_driver_merchant_id TEXT := 'lynx-bpp-0000-0000-0000-000000000000';
  v_lynx_driver_city_id TEXT := 'lynx-par-city-hels-0000-000000000000';

BEGIN
  -- Safety check: abort if this looks like production
  IF current_database() NOT IN ('atlas_dev', 'atlas_dev_test', 'nammayatri_dev') THEN
    RAISE NOTICE 'Skipping test seed: database "%" does not look like a local dev DB.', current_database();
    RETURN;
  END IF;

  SELECT id INTO v_driver_base_merchant_id FROM atlas_driver_offer_bpp.merchant WHERE subscriber_id IN ('NAMMA_YATRI', 'YATRI') LIMIT 1;
  SELECT id INTO v_driver_base_city_id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE merchant_id = v_driver_base_merchant_id LIMIT 1;

  IF v_driver_base_merchant_id IS NULL THEN
    RAISE NOTICE 'No NAMMA_YATRI merchant found in driver DB — skipping seed.';
    RETURN;
  END IF;

  RAISE NOTICE 'Base driver merchant: %, city: %', v_driver_base_merchant_id, v_driver_base_city_id;

  -- ===================== BHARAT_TAXI_PARTNER (Driver BPP) =====================

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant', 'id', v_driver_base_merchant_id, jsonb_build_object(
    'id', v_bt_driver_merchant_id,
    'name', 'Bharat Taxi Partner',
    'subscriber_id', 'BHARAT_TAXI_PARTNER',
    'short_id', 'BHARAT_TAXI_PARTNER',
    'city', 'Delhi',
    'state', 'NationalCapitalTerritory',
    'country', 'India',
    'unique_key_id', 'juspay-mobility-bpp-1-key',
    'online_payment', 'true'
  ));

  INSERT INTO atlas_driver_offer_bpp.merchant_operating_city (id, merchant_id, merchant_short_id, city, state, country,
    lat, lon, distance_unit, currency, language)
  VALUES (v_bt_driver_city_id, v_bt_driver_merchant_id, 'BHARAT_TAXI_PARTNER', 'Delhi', 'NationalCapitalTerritory', 'India',
    28.6139, 77.2090, 'Meter', 'INR', 'ENGLISH')
  ON CONFLICT (id) DO NOTHING;

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.transporter_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_driver_city_id,
    'merchant_id', v_bt_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.driver_intelligent_pool_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_driver_city_id,
    'merchant_id', v_bt_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.driver_pool_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_driver_city_id,
    'merchant_id', v_bt_driver_merchant_id,
    'id', md5(v_bt_driver_city_id || 'pool')
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant_service_usage_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_driver_city_id,
    'merchant_id', v_bt_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant_service_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_driver_city_id,
    'merchant_id', v_bt_driver_merchant_id
  ));

  -- vehicle_service_tier: clone ALL rows (can't use clone_rows — id must be unique per row)
  INSERT INTO atlas_driver_offer_bpp.vehicle_service_tier (id, air_conditioned, allowed_vehicle_variant, driver_rating, long_description, merchant_id, merchant_operating_city_id, name, seating_capacity, short_description, vehicle_rating, service_tier_type, default_for_vehicle_variant, priority, auto_selected_vehicle_variant, ventilator, oxygen, is_air_conditioned)
  SELECT md5(random()::text || vst.id), vst.air_conditioned, vst.allowed_vehicle_variant, vst.driver_rating, vst.long_description,
    v_bt_driver_merchant_id, v_bt_driver_city_id,
    vst.name, vst.seating_capacity, vst.short_description, vst.vehicle_rating, vst.service_tier_type, vst.default_for_vehicle_variant, vst.priority, vst.auto_selected_vehicle_variant, vst.ventilator, vst.oxygen, vst.is_air_conditioned
  FROM atlas_driver_offer_bpp.vehicle_service_tier vst
  WHERE vst.merchant_operating_city_id = v_driver_base_city_id
  ON CONFLICT DO NOTHING;

  -- fare_product: clone ALL enabled default fare products
  INSERT INTO atlas_driver_offer_bpp.fare_product (id, area, disable_recompute, enabled, fare_policy_id, merchant_id, merchant_operating_city_id, search_source, time_bounds, trip_category, vehicle_variant)
  SELECT md5(random()::text || fp.id), fp.area, fp.disable_recompute, fp.enabled, fp.fare_policy_id,
    v_bt_driver_merchant_id, v_bt_driver_city_id,
    fp.search_source, fp.time_bounds, fp.trip_category, fp.vehicle_variant
  FROM atlas_driver_offer_bpp.fare_product fp
  WHERE fp.merchant_operating_city_id = v_driver_base_city_id
    AND fp.enabled = true AND fp.area = 'Default' AND fp.time_bounds = 'Unbounded'
  ON CONFLICT DO NOTHING;

  RAISE NOTICE 'BHARAT_TAXI_PARTNER merchant + city created.';

  -- ===================== LYNX_PARTNER (Driver BPP) =====================

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant', 'id', v_driver_base_merchant_id, jsonb_build_object(
    'id', v_lynx_driver_merchant_id,
    'name', 'Lynx Partner',
    'subscriber_id', 'LYNX_PARTNER',
    'short_id', 'LYNX_PARTNER',
    'city', 'Helsinki',
    'state', 'Uusimaa',
    'country', 'Finland',
    'unique_key_id', 'juspay-mobility-bpp-1-key',
    'online_payment', 'true'
  ));

  INSERT INTO atlas_driver_offer_bpp.merchant_operating_city (id, merchant_id, merchant_short_id, city, state, country,
    lat, lon, distance_unit, currency, language)
  VALUES (v_lynx_driver_city_id, v_lynx_driver_merchant_id, 'LYNX_PARTNER', 'Helsinki', 'Uusimaa', 'Finland',
    60.1699, 24.9384, 'Meter', 'EUR', 'ENGLISH')
  ON CONFLICT (id) DO NOTHING;

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.transporter_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_driver_city_id,
    'merchant_id', v_lynx_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.driver_intelligent_pool_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_driver_city_id,
    'merchant_id', v_lynx_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.driver_pool_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_driver_city_id,
    'merchant_id', v_lynx_driver_merchant_id,
    'id', md5(v_lynx_driver_city_id || 'pool')
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant_service_usage_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_driver_city_id,
    'merchant_id', v_lynx_driver_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_driver_offer_bpp.merchant_service_config', 'merchant_operating_city_id', v_driver_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_driver_city_id,
    'merchant_id', v_lynx_driver_merchant_id
  ));

  -- vehicle_service_tier: clone ALL rows
  INSERT INTO atlas_driver_offer_bpp.vehicle_service_tier (id, air_conditioned, allowed_vehicle_variant, driver_rating, long_description, merchant_id, merchant_operating_city_id, name, seating_capacity, short_description, vehicle_rating, service_tier_type, default_for_vehicle_variant, priority, auto_selected_vehicle_variant, ventilator, oxygen, is_air_conditioned)
  SELECT md5(random()::text || vst.id), vst.air_conditioned, vst.allowed_vehicle_variant, vst.driver_rating, vst.long_description,
    v_lynx_driver_merchant_id, v_lynx_driver_city_id,
    vst.name, vst.seating_capacity, vst.short_description, vst.vehicle_rating, vst.service_tier_type, vst.default_for_vehicle_variant, vst.priority, vst.auto_selected_vehicle_variant, vst.ventilator, vst.oxygen, vst.is_air_conditioned
  FROM atlas_driver_offer_bpp.vehicle_service_tier vst
  WHERE vst.merchant_operating_city_id = v_driver_base_city_id
  ON CONFLICT DO NOTHING;

  -- fare_product: clone ALL enabled default fare products
  INSERT INTO atlas_driver_offer_bpp.fare_product (id, area, disable_recompute, enabled, fare_policy_id, merchant_id, merchant_operating_city_id, search_source, time_bounds, trip_category, vehicle_variant)
  SELECT md5(random()::text || fp.id), fp.area, fp.disable_recompute, fp.enabled, fp.fare_policy_id,
    v_lynx_driver_merchant_id, v_lynx_driver_city_id,
    fp.search_source, fp.time_bounds, fp.trip_category, fp.vehicle_variant
  FROM atlas_driver_offer_bpp.fare_product fp
  WHERE fp.merchant_operating_city_id = v_driver_base_city_id
    AND fp.enabled = true AND fp.area = 'Default' AND fp.time_bounds = 'Unbounded'
  ON CONFLICT DO NOTHING;

  RAISE NOTICE 'LYNX_PARTNER merchant + city created.';
END $$;

-- ===================== DRIVER PERSONS + TOKENS =====================
-- NOTE: We copy mobile_number_encrypted/hash from the base NAMMA_YATRI driver
-- so values are valid for the current Passetto instance.

-- Driver: BHARAT_TAXI_PARTNER Delhi — Sedan
INSERT INTO atlas_driver_offer_bpp.person (
  id, first_name, last_name, role, gender, identifier_type,
  mobile_number_encrypted, mobile_number_hash, mobile_country_code,
  merchant_id, merchant_operating_city_id,
  onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
  created_at, updated_at
)
SELECT
  'test-driver-bt-delhi-sedan-000000000', 'TestDriver_BT_Sedan', 'Delhi', 'DRIVER', 'MALE', 'MOBILENUMBER',
  mobile_number_encrypted, mobile_number_hash, '+91',
  'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000',
  false, false, 0, 0, 'ENGLISH',
  now(), now()
FROM atlas_driver_offer_bpp.person
WHERE merchant_id NOT IN ('bharat-t-bpp0-0000-0000-000000000000','lynx-bpp-0000-0000-0000-000000000000')
  AND mobile_number_encrypted IS NOT NULL AND role = 'DRIVER'
ORDER BY id LIMIT 1
ON CONFLICT (id) DO NOTHING;

-- Driver: BHARAT_TAXI_PARTNER Delhi — SUV (use 2nd distinct row to avoid hash collision)
INSERT INTO atlas_driver_offer_bpp.person (
  id, first_name, last_name, role, gender, identifier_type,
  mobile_number_encrypted, mobile_number_hash, mobile_country_code,
  merchant_id, merchant_operating_city_id,
  onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
  created_at, updated_at
)
SELECT
  'test-driver-bt-delhi-suv-00000000000', 'TestDriver_BT_SUV', 'Delhi', 'DRIVER', 'MALE', 'MOBILENUMBER',
  mobile_number_encrypted, mobile_number_hash, '+91',
  'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000',
  false, false, 0, 0, 'ENGLISH',
  now(), now()
FROM atlas_driver_offer_bpp.person
WHERE merchant_id NOT IN ('bharat-t-bpp0-0000-0000-000000000000','lynx-bpp-0000-0000-0000-000000000000')
  AND mobile_number_encrypted IS NOT NULL AND role = 'DRIVER'
ORDER BY id OFFSET 1 LIMIT 1
ON CONFLICT (id) DO NOTHING;

-- Driver: LYNX_PARTNER Helsinki — Sedan
INSERT INTO atlas_driver_offer_bpp.person (
  id, first_name, last_name, role, gender, identifier_type,
  mobile_number_encrypted, mobile_number_hash, mobile_country_code,
  merchant_id, merchant_operating_city_id,
  onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
  created_at, updated_at
)
SELECT
  'test-driver-lynx-hel-sedan-000000000', 'TestDriver_Lynx_Sedan', 'Helsinki', 'DRIVER', 'MALE', 'MOBILENUMBER',
  mobile_number_encrypted, mobile_number_hash, '+358',
  'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000',
  false, false, 0, 0, 'ENGLISH',
  now(), now()
FROM atlas_driver_offer_bpp.person
WHERE merchant_id NOT IN ('bharat-t-bpp0-0000-0000-000000000000','lynx-bpp-0000-0000-0000-000000000000')
  AND mobile_number_encrypted IS NOT NULL AND role = 'DRIVER'
ORDER BY id OFFSET 2 LIMIT 1
ON CONFLICT (id) DO NOTHING;

-- Driver: LYNX_PARTNER Helsinki — SUV
INSERT INTO atlas_driver_offer_bpp.person (
  id, first_name, last_name, role, gender, identifier_type,
  mobile_number_encrypted, mobile_number_hash, mobile_country_code,
  merchant_id, merchant_operating_city_id,
  onboarded_from_dashboard, is_new, total_earned_coins, used_coins, language,
  created_at, updated_at
)
SELECT
  'test-driver-lynx-hel-suv-00000000000', 'TestDriver_Lynx_SUV', 'Helsinki', 'DRIVER', 'MALE', 'MOBILENUMBER',
  mobile_number_encrypted, mobile_number_hash, '+358',
  'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000',
  false, false, 0, 0, 'ENGLISH',
  now(), now()
FROM atlas_driver_offer_bpp.person
WHERE merchant_id NOT IN ('bharat-t-bpp0-0000-0000-000000000000','lynx-bpp-0000-0000-0000-000000000000')
  AND mobile_number_encrypted IS NOT NULL AND role = 'DRIVER'
ORDER BY id OFFSET 3 LIMIT 1
ON CONFLICT (id) DO NOTHING;

-- Driver Tokens
-- NOTE: In LTS DEV mode, the token is used as driver_id directly (no auth call).
-- So token MUST equal person.id for the driver pool lookup to work.
INSERT INTO atlas_driver_offer_bpp.registration_token (
  id, auth_medium, auth_type, auth_value_hash, token,
  verified, auth_expiry, token_expiry, attempts,
  entity_id, entity_type, merchant_id,
  created_at, updated_at
) VALUES
  ('test-token-bt-drv-sedan-000000000000', 'SMS', 'OTP', '9999',
   'test-driver-bt-delhi-sedan-000000000',
   true, 3, 365, 3,
   'test-driver-bt-delhi-sedan-000000000', 'USER',
   'bharat-t-bpp0-0000-0000-000000000000',
   now(), now()),
  ('test-token-bt-drv-suv-00000000000000', 'SMS', 'OTP', '9999',
   'test-driver-bt-delhi-suv-00000000000',
   true, 3, 365, 3,
   'test-driver-bt-delhi-suv-00000000000', 'USER',
   'bharat-t-bpp0-0000-0000-000000000000',
   now(), now()),
  ('test-token-lynx-drv-sedan-0000000000', 'SMS', 'OTP', '9999',
   'test-driver-lynx-hel-sedan-000000000',
   true, 3, 365, 3,
   'test-driver-lynx-hel-sedan-000000000', 'USER',
   'lynx-bpp-0000-0000-0000-000000000000',
   now(), now()),
  ('test-token-lynx-drv-suv-000000000000', 'SMS', 'OTP', '9999',
   'test-driver-lynx-hel-suv-00000000000',
   true, 3, 365, 3,
   'test-driver-lynx-hel-suv-00000000000', 'USER',
   'lynx-bpp-0000-0000-0000-000000000000',
   now(), now())
ON CONFLICT (id) DO NOTHING;

-- ===================== DRIVER INFORMATION + VEHICLES =====================
-- Required for setActivity, nearbyRideRequest, respondQuote
-- NOTE: can_switch_to_intra_city is required for OneWay rides (getDriverInfosWithCond filter)
INSERT INTO atlas_driver_offer_bpp.driver_information (driver_id, active, enabled, verified, on_ride, blocked, num_of_locks, payment_pending, subscribed, can_downgrade_to_hatchback, can_downgrade_to_sedan, can_downgrade_to_taxi, aadhaar_verified, ac_restriction_lift_count, can_switch_to_intra_city, can_switch_to_inter_city, can_switch_to_rental, created_at, updated_at)
VALUES
  ('test-driver-bt-delhi-sedan-000000000', false, true, true, false, false, 0, false, true, false, false, false, false, 0, true, true, true, now(), now()),
  ('test-driver-bt-delhi-suv-00000000000', false, true, true, false, false, 0, false, true, false, false, false, false, 0, true, true, true, now(), now()),
  ('test-driver-lynx-hel-sedan-000000000', false, true, true, false, false, 0, false, true, false, false, false, false, 0, true, true, true, now(), now()),
  ('test-driver-lynx-hel-suv-00000000000', false, true, true, false, false, 0, false, true, false, false, false, false, 0, true, true, true, now(), now())
ON CONFLICT (driver_id) DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.vehicle (driver_id, merchant_id, variant, model, color, registration_no, category, make, capacity, vehicle_class, selected_service_tiers, created_at, updated_at)
VALUES
  ('test-driver-bt-delhi-sedan-000000000', 'bharat-t-bpp0-0000-0000-000000000000', 'SEDAN', 'Test Sedan', 'White', 'DL01AB1234', 'CAR', 'TestMake', 4, '3WT', '{SEDAN,COMFY}', now(), now()),
  ('test-driver-bt-delhi-suv-00000000000', 'bharat-t-bpp0-0000-0000-000000000000', 'SUV', 'Test SUV', 'Black', 'DL01CD5678', 'CAR', 'TestMake', 6, '3WT', '{SUV,COMFY,PREMIUM}', now(), now()),
  ('test-driver-lynx-hel-sedan-000000000', 'lynx-bpp-0000-0000-0000-000000000000', 'SEDAN', 'Test Sedan', 'Blue', 'HEL-123', 'CAR', 'TestMake', 4, '3WT', '{SEDAN,COMFY}', now(), now()),
  ('test-driver-lynx-hel-suv-00000000000', 'lynx-bpp-0000-0000-0000-000000000000', 'SUV', 'Test SUV', 'Black', 'HEL-456', 'CAR', 'TestMake', 6, '3WT', '{SUV,COMFY,PREMIUM}', now(), now())
ON CONFLICT (driver_id) DO NOTHING;

-- driver_stats (required for respondQuote and other driver flows)
INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id, bonus_earned, earnings_missed, idle_since, late_night_trips, total_distance, total_earnings, total_rides, fav_rider_count, updated_at)
VALUES
  ('test-driver-bt-delhi-sedan-000000000', 0, 0, now(), 0, 0, 0, 0, 0, now()),
  ('test-driver-bt-delhi-suv-00000000000', 0, 0, now(), 0, 0, 0, 0, 0, now()),
  ('test-driver-lynx-hel-sedan-000000000', 0, 0, now(), 0, 0, 0, 0, 0, now()),
  ('test-driver-lynx-hel-suv-00000000000', 0, 0, now(), 0, 0, 0, 0, 0, now())
ON CONFLICT (driver_id) DO NOTHING;

-- go_home_config (required for initiateDriverSearchBatch)
INSERT INTO atlas_driver_offer_bpp.go_home_config (merchant_id, merchant_operating_city_id)
VALUES
  ('lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000'),
  ('bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000')
ON CONFLICT (merchant_operating_city_id) DO NOTHING;

-- value_add_np (required for on_confirm to send driverAccountId for online payment)
INSERT INTO atlas_driver_offer_bpp.value_add_np (subscriber_id, enabled, created_at, updated_at)
VALUES ('lynx-bap-id', true, now(), now()), ('bharat-taxi-bap-id', true, now(), now())
ON CONFLICT DO NOTHING;

-- exophone (required for init/confirm — virtual phone number for driver-rider masking)
INSERT INTO atlas_driver_offer_bpp.exophone (id, merchant_id, merchant_operating_city_id, primary_phone, backup_phone, is_primary_down, call_service, created_at, updated_at)
VALUES
  (md5('lynx-exophone')::uuid, 'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000', '+910000000001', '+910000000001', false, 'Exotel', now(), now()),
  (md5('bt-exophone')::uuid, 'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000', '+910000000001', '+910000000001', false, 'Exotel', now(), now())
ON CONFLICT DO NOTHING;

-- merchant_payment_method (required for init/confirm — payment method validation)
DO $$
DECLARE v_base_city_id TEXT;
BEGIN
  SELECT id INTO v_base_city_id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE subscriber_id IN ('NAMMA_YATRI','YATRI') LIMIT 1) LIMIT 1;

  INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority, updated_at, created_at)
  SELECT md5(random()::text)::uuid, 'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000',
    payment_type, payment_instrument, collected_by, priority, now(), now()
  FROM atlas_driver_offer_bpp.merchant_payment_method WHERE merchant_operating_city_id = v_base_city_id ON CONFLICT DO NOTHING;

  INSERT INTO atlas_driver_offer_bpp.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority, updated_at, created_at)
  SELECT md5(random()::text)::uuid, 'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000',
    payment_type, payment_instrument, collected_by, priority, now(), now()
  FROM atlas_driver_offer_bpp.merchant_payment_method WHERE merchant_operating_city_id = v_base_city_id ON CONFLICT DO NOTHING;
END $$;

-- merchant_push_notification (required for sending ride request notifications to drivers)
DO $$
DECLARE v_base_city_id TEXT;
BEGIN
  SELECT id INTO v_base_city_id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE subscriber_id IN ('NAMMA_YATRI','YATRI') LIMIT 1) LIMIT 1;

  INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
    (id, fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at)
  SELECT md5(random()::text || id)::uuid, fcm_notification_type, key,
    'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000',
    title, body, language, now(), now()
  FROM atlas_driver_offer_bpp.merchant_push_notification WHERE merchant_operating_city_id = v_base_city_id
  ON CONFLICT DO NOTHING;

  INSERT INTO atlas_driver_offer_bpp.merchant_push_notification
    (id, fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at)
  SELECT md5(random()::text || id)::uuid, fcm_notification_type, key,
    'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000',
    title, body, language, now(), now()
  FROM atlas_driver_offer_bpp.merchant_push_notification WHERE merchant_operating_city_id = v_base_city_id
  ON CONFLICT DO NOTHING;
END $$;

INSERT INTO atlas_driver_offer_bpp.driver_bank_account (account_id, charges_enabled, details_submitted, driver_id, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES
  ('acct_test_bt_sedan', true, true, 'test-driver-bt-delhi-sedan-000000000', 'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000', now(), now()),
  ('acct_test_bt_suv', true, true, 'test-driver-bt-delhi-suv-00000000000', 'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000', now(), now()),
  ('acct_test_lynx_sedan', true, true, 'test-driver-lynx-hel-sedan-000000000', 'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000', now(), now()),
  ('acct_test_lynx_suv', true, true, 'test-driver-lynx-hel-suv-00000000000', 'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000', now(), now())
ON CONFLICT (driver_id) DO NOTHING;

-- =============================================================================
-- Point ALL Stripe payment configs to Mock Stripe server (localhost:7081)
-- =============================================================================
UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = jsonb_set(
  jsonb_set(
    jsonb_set(config_json::jsonb, '{url}', '"http://localhost:7081"'),
    '{webhookEndpointSecret}', '"0.1.0|3|BynFRjXo5OEX6kJLMNJAQWGmqxgbAnaCy+PzYUqvvT8ud51nVXU3u+r+MA/rsSWyLwZv8f6JpsrOunfF984jVkK/OWQ0Ww=="'
  ),
  '{serviceMode}', '"Test"'
)
WHERE service_name LIKE 'Payment_Stripe%';

-- Driver app geometry (no SRID — beam queries use ST_GeomFromText without SRID)

-- Ernakulam / Kochi area (covers Kochi city + surroundings)
INSERT INTO atlas_driver_offer_bpp.geometry (id, region, city, state, geom)
SELECT 'ernakulam-geom-driver-00000000000', 'Ernakulam', 'Kochi', 'Kerala',
  ST_GeomFromText('MULTIPOLYGON(((
    76.1000 9.8000, 76.2000 9.8000, 76.4000 9.8500, 76.5000 9.9000,
    76.5500 10.0000, 76.5500 10.1500, 76.5000 10.2500, 76.4000 10.3500,
    76.3000 10.4000, 76.2000 10.4000, 76.1000 10.3500, 76.0500 10.2500,
    76.0000 10.1000, 76.0000 9.9500, 76.1000 9.8000
  )))')
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.geometry WHERE id = 'ernakulam-geom-driver-00000000000');

-- Kerala state (broad coverage for inter-city)
INSERT INTO atlas_driver_offer_bpp.geometry (id, region, city, state, geom)
SELECT 'kerala-geom-driver-000000000000000', 'Kerala', 'Kochi', 'Kerala',
  ST_GeomFromText('MULTIPOLYGON(((
    74.8000 8.2000, 75.2000 8.2000, 75.8000 8.5000, 76.5000 9.0000,
    76.8000 9.5000, 77.2000 10.0000, 77.4000 10.5000, 77.2000 11.0000,
    77.0000 11.5000, 76.5000 12.0000, 76.0000 12.5000, 75.5000 12.5000,
    75.0000 12.0000, 74.8000 11.5000, 74.5000 11.0000, 74.5000 10.5000,
    74.6000 10.0000, 74.7000 9.5000, 74.8000 9.0000, 74.8000 8.2000
  )))')
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.geometry WHERE id = 'kerala-geom-driver-000000000000000');

-- Delhi NCR boundary
INSERT INTO atlas_driver_offer_bpp.geometry (id, region, city, state, geom)
SELECT 'delhi-geometry-0000-000000000001', 'Delhi NCR', 'Delhi', 'Delhi',
  ST_GeomFromText('MULTIPOLYGON(((
    76.8400 28.4000, 77.0000 28.4000, 77.1500 28.4500, 77.3500 28.5000,
    77.4000 28.5500, 77.4000 28.6500, 77.3800 28.7500, 77.3500 28.8500,
    77.2500 28.8800, 77.1000 28.8800, 76.9500 28.8500, 76.8500 28.7500,
    76.8200 28.6500, 76.8200 28.5500, 76.8400 28.4000
  )))')
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.geometry WHERE id = 'delhi-geometry-0000-000000000001');

-- Helsinki metropolitan area boundary
INSERT INTO atlas_driver_offer_bpp.geometry (id, region, city, state, geom)
SELECT 'helsinki-geometry-000-000000000001', 'Helsinki Metropolitan', 'Helsinki', 'Uusimaa',
  ST_GeomFromText('MULTIPOLYGON(((
    24.5000 60.0500, 24.7000 60.0500, 24.9000 60.0800, 25.1000 60.1000,
    25.2500 60.1500, 25.3000 60.2500, 25.2500 60.3500, 25.1500 60.4000,
    25.0000 60.4200, 24.8000 60.4000, 24.6000 60.3500, 24.5000 60.2500,
    24.4500 60.1500, 24.5000 60.0500
  )))')
WHERE NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.geometry WHERE id = 'helsinki-geometry-000-000000000001');

-- =============================================================================
-- Beckn Config for new BPP merchants (ALL vehicle categories + TTL fields)
-- =============================================================================
INSERT INTO atlas_driver_offer_bpp.beckn_config (
  id, subscriber_id, subscriber_url, domain, gateway_url, registry_url,
  unique_key_id, vehicle_category, merchant_id, merchant_operating_city_id,
  collected_by, payment_params_json,
  on_search_ttl_sec, on_select_ttl_sec, on_init_ttl_sec, on_confirm_ttl_sec,
  on_cancel_ttl_sec, on_status_ttl_sec, on_track_ttl_sec, on_update_ttl_sec,
  multimodal_on_search_ttl_sec, buyer_finder_fee, settlement_type, settlement_window, static_terms_url,
  created_at, updated_at)
SELECT md5(random()::text || bc.id)::uuid, 'LYNX_PARTNER',
  'http://localhost:8013/beckn/cab/v1/lynx-bap-0000-0000-0000-000000000000',
  bc.domain, bc.gateway_url, bc.registry_url, bc.unique_key_id, bc.vehicle_category,
  'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000',
  bc.collected_by, bc.payment_params_json,
  bc.on_search_ttl_sec, bc.on_select_ttl_sec, bc.on_init_ttl_sec, bc.on_confirm_ttl_sec,
  bc.on_cancel_ttl_sec, bc.on_status_ttl_sec, bc.on_track_ttl_sec, bc.on_update_ttl_sec,
  bc.multimodal_on_search_ttl_sec, bc.buyer_finder_fee, bc.settlement_type, bc.settlement_window, bc.static_terms_url,
  now(), now()
FROM atlas_driver_offer_bpp.beckn_config bc
WHERE bc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE subscriber_id = 'NAMMA_YATRI' LIMIT 1)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.beckn_config (
  id, subscriber_id, subscriber_url, domain, gateway_url, registry_url,
  unique_key_id, vehicle_category, merchant_id, merchant_operating_city_id,
  collected_by, payment_params_json,
  on_search_ttl_sec, on_select_ttl_sec, on_init_ttl_sec, on_confirm_ttl_sec,
  on_cancel_ttl_sec, on_status_ttl_sec, on_track_ttl_sec, on_update_ttl_sec,
  multimodal_on_search_ttl_sec, buyer_finder_fee, settlement_type, settlement_window, static_terms_url,
  created_at, updated_at)
SELECT md5(random()::text || bc.id)::uuid, 'BHARAT_TAXI_PARTNER',
  'http://localhost:8013/beckn/cab/v1/bharat-t-bap0-0000-0000-000000000000',
  bc.domain, bc.gateway_url, bc.registry_url, bc.unique_key_id, bc.vehicle_category,
  'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000',
  bc.collected_by, bc.payment_params_json,
  bc.on_search_ttl_sec, bc.on_select_ttl_sec, bc.on_init_ttl_sec, bc.on_confirm_ttl_sec,
  bc.on_cancel_ttl_sec, bc.on_status_ttl_sec, bc.on_track_ttl_sec, bc.on_update_ttl_sec,
  bc.multimodal_on_search_ttl_sec, bc.buyer_finder_fee, bc.settlement_type, bc.settlement_window, bc.static_terms_url,
  now(), now()
FROM atlas_driver_offer_bpp.beckn_config bc
WHERE bc.merchant_id = (SELECT id FROM atlas_driver_offer_bpp.merchant WHERE subscriber_id = 'NAMMA_YATRI' LIMIT 1)
ON CONFLICT DO NOTHING;

-- =============================================================================
-- White List Org: Allow new BAP subscribers to send search to new BPP merchants
-- =============================================================================
INSERT INTO atlas_driver_offer_bpp.white_list_org (id, subscriber_id, domain, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES (md5(random()::text)::uuid, 'lynx-bap-id', 'MOBILITY', 'lynx-bpp-0000-0000-0000-000000000000', 'lynx-par-city-hels-0000-000000000000', now(), now())
ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.white_list_org (id, subscriber_id, domain, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES (md5(random()::text)::uuid, 'bharat-taxi-bap-id', 'MOBILITY', 'bharat-t-bpp0-0000-0000-000000000000', 'bt-partn-city-delh-0000-000000000000', now(), now())
ON CONFLICT DO NOTHING;

-- =============================================================================
-- Fix geofencing restrictions to match geometry regions
-- =============================================================================
UPDATE atlas_driver_offer_bpp.merchant SET origin_restriction = '{Ernakulam,Kerala}', destination_restriction = '{Ernakulam,Kerala}' WHERE short_id = 'NAMMA_YATRI_PARTNER';
UPDATE atlas_driver_offer_bpp.merchant SET origin_restriction = '{Ernakulam,Kerala}', destination_restriction = '{Ernakulam,Kerala}' WHERE short_id = 'OTHER_MERCHANT_2';
UPDATE atlas_driver_offer_bpp.merchant SET origin_restriction = '{Delhi NCR}', destination_restriction = '{Delhi NCR}' WHERE short_id = 'BHARAT_TAXI_PARTNER';
UPDATE atlas_driver_offer_bpp.merchant SET origin_restriction = '{Helsinki Metropolitan}', destination_restriction = '{Helsinki Metropolitan}' WHERE short_id = 'LYNX_PARTNER';

-- =============================================================================
-- Driver Wallet + Commission for LYNX_PARTNER
-- =============================================================================

-- Enable wallet on merchant
UPDATE atlas_driver_offer_bpp.merchant
SET prepaid_subscription_and_wallet_enabled = true
WHERE short_id = 'LYNX_PARTNER';

-- Enable driver wallet in transporter_config (no forceOnlineLedger, no TDS)
UPDATE atlas_driver_offer_bpp.transporter_config
SET driver_wallet_config = jsonb_set(
  jsonb_set(
    driver_wallet_config::jsonb,
    '{enableDriverWallet}', 'true'
  ),
  '{gstPercentage}', '18.0'
)
WHERE merchant_operating_city_id = 'lynx-par-city-hels-0000-000000000000';

-- Create separate fare policy for Lynx SUV with 10% commission via commissionChargeConfig
-- Also clone all progressive details child tables
DO $$
DECLARE
  v_base_fp_id TEXT;
  v_new_fp_id TEXT := md5('lynx-suv-fare-policy');
  cols TEXT;
BEGIN
  SELECT fp.id INTO v_base_fp_id
  FROM atlas_driver_offer_bpp.fare_policy fp
  JOIN atlas_driver_offer_bpp.fare_product fpr ON fpr.fare_policy_id = fp.id
  WHERE fpr.merchant_operating_city_id = 'lynx-par-city-hels-0000-000000000000'
    AND fpr.vehicle_variant = 'SUV'
    AND fpr.trip_category = 'OneWay_OneWayOnDemandDynamicOffer'
  LIMIT 1;

  IF v_base_fp_id IS NOT NULL AND v_base_fp_id != v_new_fp_id THEN
    -- Clone fare_policy
    SELECT string_agg(
      CASE WHEN column_name = 'id' THEN quote_literal(v_new_fp_id) || '::text'
      ELSE quote_ident(column_name) END, ', '
      ORDER BY ordinal_position
    ) INTO cols
    FROM information_schema.columns
    WHERE table_schema = 'atlas_driver_offer_bpp' AND table_name = 'fare_policy';

    EXECUTE format(
      'INSERT INTO atlas_driver_offer_bpp.fare_policy SELECT %s FROM atlas_driver_offer_bpp.fare_policy WHERE id = %L ON CONFLICT (id) DO NOTHING',
      cols, v_base_fp_id
    );

    -- Set 10% commission on RideFare
    UPDATE atlas_driver_offer_bpp.fare_policy
    SET commission_charge_config = '{"value":"10%","appliesOn":["RideFare", "ExtraKmFareComponent"]}',
        platform_fee_charges_by = 'None'
    WHERE id = v_new_fp_id;

    -- Clone progressive details
    INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details
    SELECT v_new_fp_id, base_distance, base_fare, dead_km_fare, waiting_charge,
      night_shift_charge, free_wating_time, currency, base_fare_amount, dead_km_fare_amount,
      distance_unit, pickup_charges_min, pickup_charges_max, pickup_charges_min_amount, pickup_charges_max_amount
    FROM atlas_driver_offer_bpp.fare_policy_progressive_details WHERE fare_policy_id = v_base_fp_id
    ON CONFLICT (fare_policy_id) DO NOTHING;

    -- Clone per extra km rate sections
    INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section
      (fare_policy_id, start_distance, per_extra_km_rate, distance_unit, base_fare_depreciation)
    SELECT v_new_fp_id, start_distance, per_extra_km_rate, distance_unit, base_fare_depreciation
    FROM atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section WHERE fare_policy_id = v_base_fp_id
    ON CONFLICT DO NOTHING;

    -- Clone per min rate sections
    INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details_per_min_rate_section
      (currency, fare_policy_id, per_min_rate, ride_duration_in_min, created_at, updated_at)
    SELECT currency, v_new_fp_id, per_min_rate, ride_duration_in_min, now(), now()
    FROM atlas_driver_offer_bpp.fare_policy_progressive_details_per_min_rate_section WHERE fare_policy_id = v_base_fp_id
    ON CONFLICT DO NOTHING;

    -- Clone driver extra fee bounds
    INSERT INTO atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds
      (fare_policy_id, start_distance, min_fee, max_fee, step_fee, default_step_fee,
       min_fee_amount, max_fee_amount, step_fee_amount, default_step_fee_amount, distance_unit)
    SELECT v_new_fp_id, start_distance, min_fee, max_fee, step_fee, default_step_fee,
       min_fee_amount, max_fee_amount, step_fee_amount, default_step_fee_amount, distance_unit
    FROM atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds WHERE fare_policy_id = v_base_fp_id
    ON CONFLICT DO NOTHING;

    -- Point Lynx SUV fare_product to new fare_policy
    UPDATE atlas_driver_offer_bpp.fare_product
    SET fare_policy_id = v_new_fp_id
    WHERE merchant_operating_city_id = 'lynx-par-city-hels-0000-000000000000'
      AND vehicle_variant = 'SUV'
      AND trip_category = 'OneWay_OneWayOnDemandDynamicOffer';
  END IF;
END $$;
