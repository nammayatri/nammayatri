-- WARNING: LOCAL DEVELOPMENT ONLY — DO NOT RUN IN PRODUCTION
-- =============================================================================
-- Test Seed Data (Rider / atlas_app): Merchants, Cities, Persons, Tokens
--
-- Strategy: Clone all config rows from base YATRI merchant using dynamic SQL
-- to avoid hardcoding column lists (which break when columns are added).
--
-- Creates:
--   BHARAT_TAXI (BAP) — Delhi, INR
--   LYNX (BAP) — Helsinki, EUR
--   Test riders + registration tokens for each merchant
--
-- Prerequisites: YATRI/NAMMA_YATRI merchant must already exist (from base seed).
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
  -- Get all column names for the table
  SELECT array_agg(column_name::text ORDER BY ordinal_position)
  INTO cols
  FROM information_schema.columns
  WHERE table_schema || '.' || table_name = p_table;

  -- Build SELECT list: use replacement value if specified, otherwise original column
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

  -- BHARAT_TAXI IDs
  v_bt_rider_merchant_id TEXT := 'bharat-t-bap0-0000-0000-000000000000';
  v_bt_rider_city_id TEXT := 'bharat-t-city-delh-0000-000000000000';
  v_bt_driver_merchant_id TEXT := 'bharat-t-bpp0-0000-0000-000000000000';

  -- LYNX IDs
  v_lynx_rider_merchant_id TEXT := 'lynx-bap-0000-0000-0000-000000000000';
  v_lynx_rider_city_id TEXT := 'lynx-cit-hels-inki-0000-000000000000';
  v_lynx_driver_merchant_id TEXT := 'lynx-bpp-0000-0000-0000-000000000000';

BEGIN
  -- Safety check: abort if this looks like production
  IF current_database() NOT IN ('atlas_dev', 'atlas_dev_test', 'nammayatri_dev') THEN
    RAISE NOTICE 'Skipping test seed: database "%" does not look like a local dev DB.', current_database();
    RETURN;
  END IF;

  SELECT id INTO v_rider_base_merchant_id FROM atlas_app.merchant WHERE short_id IN ('YATRI', 'NAMMA_YATRI') LIMIT 1;
  SELECT id INTO v_rider_base_city_id FROM atlas_app.merchant_operating_city WHERE merchant_id = v_rider_base_merchant_id LIMIT 1;

  IF v_rider_base_merchant_id IS NULL THEN
    RAISE NOTICE 'No YATRI merchant found in rider DB — skipping seed.';
    RETURN;
  END IF;

  RAISE NOTICE 'Base rider merchant: %, city: %', v_rider_base_merchant_id, v_rider_base_city_id;

  -- ===================== BHARAT_TAXI (Rider BAP) =====================

  -- Clone merchant row, override identity fields
  PERFORM pg_temp.clone_rows('atlas_app.merchant', 'id', v_rider_base_merchant_id, jsonb_build_object(
    'id', v_bt_rider_merchant_id,
    'short_id', 'BHARAT_TAXI',
    'fallback_short_id', 'BHARAT_TAXI',
    'subscriber_id', 'bharat-taxi-bap-sub',
    'name', 'Bharat Taxi',
    'city', 'Delhi',
    'country', 'India',
    'bap_id', 'bharat-taxi-bap-id',
    'bap_unique_key_id', 'bharat-taxi-bap-key',
    'driver_offer_merchant_id', v_bt_driver_merchant_id,
    'online_payment', 'true'
  ));

  INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city, state, country,
    lat, long, distance_unit, created_at, updated_at)
  VALUES (v_bt_rider_city_id, v_bt_rider_merchant_id, 'BHARAT_TAXI', 'Delhi', 'NationalCapitalTerritory', 'India',
    28.6139, 77.2090, 'Meter', now(), now())
  ON CONFLICT (id) DO NOTHING;

  -- Clone rider_config
  PERFORM pg_temp.clone_rows('atlas_app.rider_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_rider_city_id,
    'merchant_id', v_bt_rider_merchant_id
  ));

  -- Clone merchant_service_usage_config
  PERFORM pg_temp.clone_rows('atlas_app.merchant_service_usage_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_rider_city_id,
    'merchant_id', v_bt_rider_merchant_id
  ));

  -- Clone merchant_service_config rows
  PERFORM pg_temp.clone_rows('atlas_app.merchant_service_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_rider_city_id,
    'merchant_id', v_bt_rider_merchant_id
  ));

  -- Clone merchant_payment_method rows
  PERFORM pg_temp.clone_rows('atlas_app.merchant_payment_method', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_bt_rider_city_id,
    'merchant_id', v_bt_rider_merchant_id,
    'id', md5(v_bt_rider_city_id || 'payment_method')
  ));

  RAISE NOTICE 'BHARAT_TAXI rider merchant + city created.';

  -- ===================== LYNX (Rider BAP) =====================

  PERFORM pg_temp.clone_rows('atlas_app.merchant', 'id', v_rider_base_merchant_id, jsonb_build_object(
    'id', v_lynx_rider_merchant_id,
    'short_id', 'LYNX',
    'fallback_short_id', 'LYNX',
    'subscriber_id', 'lynx-bap-sub',
    'name', 'Lynx Rides',
    'city', 'Helsinki',
    'country', 'Finland',
    'bap_id', 'lynx-bap-id',
    'bap_unique_key_id', 'lynx-bap-key',
    'driver_offer_merchant_id', v_lynx_driver_merchant_id,
    'online_payment', 'true'
  ));

  INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city, state, country,
    lat, long, distance_unit, created_at, updated_at)
  VALUES (v_lynx_rider_city_id, v_lynx_rider_merchant_id, 'LYNX', 'Helsinki', 'Karnataka', 'Finland',
    60.1699, 24.9384, 'Meter', now(), now())
  ON CONFLICT (id) DO NOTHING;

  PERFORM pg_temp.clone_rows('atlas_app.rider_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_rider_city_id,
    'merchant_id', v_lynx_rider_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_app.merchant_service_usage_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_rider_city_id,
    'merchant_id', v_lynx_rider_merchant_id
  ));

  PERFORM pg_temp.clone_rows('atlas_app.merchant_service_config', 'merchant_operating_city_id', v_rider_base_city_id, jsonb_build_object(
    'merchant_operating_city_id', v_lynx_rider_city_id,
    'merchant_id', v_lynx_rider_merchant_id
  ));

  RAISE NOTICE 'LYNX rider merchant + city created.';
END $$;

-- ===================== RIDER PERSONS + TOKENS =====================
-- NOTE: We copy mobile_number_encrypted/hash from the base YATRI rider so the
-- values are valid for the current Passetto instance (hardcoded ciphertext
-- breaks when Passetto keys differ between machines).

INSERT INTO atlas_app.person (
  id, first_name, role, gender, identifier_type,
  mobile_number_encrypted, mobile_number_hash, mobile_country_code,
  merchant_id, merchant_operating_city_id,
  is_new, enabled, blocked, has_taken_valid_ride, is_valid_rating,
  created_at, updated_at
)
SELECT
  'test-rider-bharat-taxi-delhi-0000000', 'TestRider_BharatTaxi', 'USER', 'UNKNOWN', 'MOBILENUMBER',
  mobile_number_encrypted, mobile_number_hash, '+91',
  'bharat-t-bap0-0000-0000-000000000000', 'bharat-t-city-delh-0000-000000000000',
  false, true, false, false, true,
  now(), now()
FROM atlas_app.person
WHERE merchant_id NOT IN ('bharat-t-bap0-0000-0000-000000000000','lynx-bap-0000-0000-0000-000000000000')
  AND mobile_number_encrypted IS NOT NULL
ORDER BY id LIMIT 1
ON CONFLICT (id) DO NOTHING;

INSERT INTO atlas_app.person (
  id, first_name, role, gender, identifier_type,
  mobile_number_encrypted, mobile_number_hash, mobile_country_code,
  merchant_id, merchant_operating_city_id,
  is_new, enabled, blocked, has_taken_valid_ride, is_valid_rating,
  created_at, updated_at
)
SELECT
  'test-rider-lynx-helsinki-00000000000', 'TestRider_Lynx', 'USER', 'UNKNOWN', 'MOBILENUMBER',
  mobile_number_encrypted, mobile_number_hash, '+358',
  'lynx-bap-0000-0000-0000-000000000000', 'lynx-cit-hels-inki-0000-000000000000',
  false, true, false, false, true,
  now(), now()
FROM atlas_app.person
WHERE merchant_id NOT IN ('bharat-t-bap0-0000-0000-000000000000','lynx-bap-0000-0000-0000-000000000000')
  AND mobile_number_encrypted IS NOT NULL
ORDER BY id OFFSET 1 LIMIT 1
ON CONFLICT (id) DO NOTHING;

-- Rider Tokens
INSERT INTO atlas_app.registration_token (
  id, auth_medium, auth_type, auth_value_hash, token,
  verified, auth_expiry, token_expiry, attempts,
  entity_id, entity_type, merchant_id,
  created_at, updated_at
) VALUES
  ('test-token-bt-rider-0000000000000000', 'SMS', 'OTP', '9999',
   'test-token-bharat-taxi-rider-delhi',
   true, 3, 365, 3,
   'test-rider-bharat-taxi-delhi-0000000', 'USER',
   'bharat-t-bap0-0000-0000-000000000000',
   now(), now()),
  ('test-token-lynx-rider-00000000000000', 'SMS', 'OTP', '9999',
   'test-token-lynx-rider-helsinki',
   true, 3, 365, 3,
   'test-rider-lynx-helsinki-00000000000', 'USER',
   'lynx-bap-0000-0000-0000-000000000000',
   now(), now())
ON CONFLICT (id) DO NOTHING;

-- =============================================================================
-- Point ALL Stripe payment configs to Mock Stripe server (localhost:7081)
-- This enables local testing without a real Stripe account.
-- =============================================================================
UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
  jsonb_set(
    jsonb_set(config_json::jsonb, '{url}', '"http://localhost:7081"'),
    '{webhookEndpointSecret}', '"0.1.0|3|BynFRjXo5OEX6kJLMNJAQWGmqxgbAnaCy+PzYUqvvT8ud51nVXU3u+r+MA/rsSWyLwZv8f6JpsrOunfF984jVkK/OWQ0Ww=="'
  ),
  '{serviceMode}', '"Test"'
)
WHERE service_name LIKE 'Payment_Stripe%';

-- =============================================================================
-- HotSpotConfig for new merchants (required for rideSearch)
-- =============================================================================
INSERT INTO atlas_app.hot_spot_config (block_radius, hot_spot_expiry, hot_spot_radius, id,
  max_geo_hash_to_filter, max_num_hot_spots_to_show, min_frequency_of_hot_spot,
  precision_to_filter_geohash, precision_to_get_geohash, precision_to_set_geohash,
  should_save_search_hot_spot, should_take_hot_spot,
  weight_of_auto_pickup, weight_of_auto_saved, weight_of_manual_pickup,
  weight_of_manual_saved, weight_of_special_location, weight_of_trip_end, weight_of_trip_start)
SELECT block_radius, hot_spot_expiry, hot_spot_radius, 'bharat-t-bap0-0000-0000-000000000000',
  max_geo_hash_to_filter, max_num_hot_spots_to_show, min_frequency_of_hot_spot,
  precision_to_filter_geohash, precision_to_get_geohash, precision_to_set_geohash,
  should_save_search_hot_spot, should_take_hot_spot,
  weight_of_auto_pickup, weight_of_auto_saved, weight_of_manual_pickup,
  weight_of_manual_saved, weight_of_special_location, weight_of_trip_end, weight_of_trip_start
FROM atlas_app.hot_spot_config WHERE id = (SELECT id FROM atlas_app.hot_spot_config LIMIT 1)
ON CONFLICT (id) DO NOTHING;

INSERT INTO atlas_app.hot_spot_config (block_radius, hot_spot_expiry, hot_spot_radius, id,
  max_geo_hash_to_filter, max_num_hot_spots_to_show, min_frequency_of_hot_spot,
  precision_to_filter_geohash, precision_to_get_geohash, precision_to_set_geohash,
  should_save_search_hot_spot, should_take_hot_spot,
  weight_of_auto_pickup, weight_of_auto_saved, weight_of_manual_pickup,
  weight_of_manual_saved, weight_of_special_location, weight_of_trip_end, weight_of_trip_start)
SELECT block_radius, hot_spot_expiry, hot_spot_radius, 'lynx-bap-0000-0000-0000-000000000000',
  max_geo_hash_to_filter, max_num_hot_spots_to_show, min_frequency_of_hot_spot,
  precision_to_filter_geohash, precision_to_get_geohash, precision_to_set_geohash,
  should_save_search_hot_spot, should_take_hot_spot,
  weight_of_auto_pickup, weight_of_auto_saved, weight_of_manual_pickup,
  weight_of_manual_saved, weight_of_special_location, weight_of_trip_end, weight_of_trip_start
FROM atlas_app.hot_spot_config WHERE id = (SELECT id FROM atlas_app.hot_spot_config LIMIT 1)
ON CONFLICT (id) DO NOTHING;

-- =============================================================================

-- =============================================================================
-- Geofencing: Update origin/destination restrictions to match city geometry regions
-- =============================================================================
UPDATE atlas_app.merchant
SET origin_restriction = '{Helsinki Metropolitan}',
    destination_restriction = '{Helsinki Metropolitan}'
WHERE id = 'lynx-bap-0000-0000-0000-000000000000';

UPDATE atlas_app.merchant
SET origin_restriction = '{Delhi NCR}',
    destination_restriction = '{Delhi NCR}'
WHERE id = 'bharat-t-bap0-0000-0000-000000000000';

-- =============================================================================
-- White List Org: Allow BPP on_search callbacks from new merchants
-- =============================================================================
INSERT INTO atlas_app.white_list_org (id, subscriber_id, domain, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES
  (md5(random()::text)::uuid, 'LYNX_PARTNER', 'MOBILITY', 'lynx-bap-0000-0000-0000-000000000000', 'lynx-cit-hels-inki-0000-000000000000', now(), now()),
  (md5(random()::text)::uuid, 'BHARAT_TAXI_PARTNER', 'MOBILITY', 'bharat-t-bap0-0000-0000-000000000000', 'bharat-t-city-delh-0000-000000000000', now(), now())
ON CONFLICT DO NOTHING;

-- =============================================================================
-- Beckn Config for new merchants (required for search → BPP communication)
-- =============================================================================
-- Clone ALL beckn_config entries (all vehicle categories + domains, with TTLs)
INSERT INTO atlas_app.beckn_config (
  id, bap_ifsc, buyer_finder_fee, collected_by, confirm_buffer_ttl_sec, confirm_ttl_sec,
  domain, gateway_url, init_ttl_sec, payment_params_json, registry_url,
  search_ttl_sec, select_ttl_sec, settlement_type, settlement_window, static_terms_url,
  subscriber_id, subscriber_url, unique_key_id, vehicle_category,
  merchant_id, merchant_operating_city_id, created_at, updated_at,
  track_ttl_sec, status_ttl_sec, rating_ttl_sec)
SELECT md5(random()::text || bc.id)::uuid, bc.bap_ifsc, bc.buyer_finder_fee, bc.collected_by, bc.confirm_buffer_ttl_sec, bc.confirm_ttl_sec,
  bc.domain, bc.gateway_url, bc.init_ttl_sec, bc.payment_params_json, bc.registry_url,
  bc.search_ttl_sec, bc.select_ttl_sec, bc.settlement_type, bc.settlement_window, bc.static_terms_url,
  'localhost:8013/beckn/cab/v1/bharat-t-bap0-0000-0000-000000000000',
  'http://localhost:8013/beckn/cab/v1/bharat-t-bap0-0000-0000-000000000000',
  bc.unique_key_id, bc.vehicle_category,
  'bharat-t-bap0-0000-0000-000000000000', 'bharat-t-city-delh-0000-000000000000', now(), now(),
  bc.track_ttl_sec, bc.status_ttl_sec, bc.rating_ttl_sec
FROM atlas_app.beckn_config bc
WHERE bc.merchant_id = (SELECT id FROM atlas_app.merchant WHERE short_id IN ('YATRI','NAMMA_YATRI') LIMIT 1)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_app.beckn_config (
  id, bap_ifsc, buyer_finder_fee, collected_by, confirm_buffer_ttl_sec, confirm_ttl_sec,
  domain, gateway_url, init_ttl_sec, payment_params_json, registry_url,
  search_ttl_sec, select_ttl_sec, settlement_type, settlement_window, static_terms_url,
  subscriber_id, subscriber_url, unique_key_id, vehicle_category,
  merchant_id, merchant_operating_city_id, created_at, updated_at,
  track_ttl_sec, status_ttl_sec, rating_ttl_sec)
SELECT md5(random()::text || bc.id)::uuid, bc.bap_ifsc, bc.buyer_finder_fee, bc.collected_by, bc.confirm_buffer_ttl_sec, bc.confirm_ttl_sec,
  bc.domain, bc.gateway_url, bc.init_ttl_sec, bc.payment_params_json, bc.registry_url,
  bc.search_ttl_sec, bc.select_ttl_sec, bc.settlement_type, bc.settlement_window, bc.static_terms_url,
  'localhost:8013/beckn/cab/v1/lynx-bap-0000-0000-0000-000000000000',
  'http://localhost:8013/beckn/cab/v1/lynx-bap-0000-0000-0000-000000000000',
  bc.unique_key_id, bc.vehicle_category,
  'lynx-bap-0000-0000-0000-000000000000', 'lynx-cit-hels-inki-0000-000000000000', now(), now(),
  bc.track_ttl_sec, bc.status_ttl_sec, bc.rating_ttl_sec
FROM atlas_app.beckn_config bc
WHERE bc.merchant_id = (SELECT id FROM atlas_app.merchant WHERE short_id IN ('YATRI','NAMMA_YATRI') LIMIT 1)
ON CONFLICT DO NOTHING;

-- value_add_np (required for on_confirm to handle online payment fields from BPP)
INSERT INTO atlas_app.value_add_np (subscriber_id, enabled, created_at, updated_at)
VALUES ('LYNX_PARTNER', true, now(), now()), ('BHARAT_TAXI_PARTNER', true, now(), now())
ON CONFLICT DO NOTHING;

-- merchant_payment_method (required for init/confirm — payment method validation)
DO $$
DECLARE v_base_city_id TEXT;
BEGIN
  SELECT id INTO v_base_city_id FROM atlas_app.merchant_operating_city
  WHERE merchant_id = (SELECT id FROM atlas_app.merchant WHERE short_id IN ('YATRI','NAMMA_YATRI') LIMIT 1) LIMIT 1;

  INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority, updated_at, created_at)
  SELECT md5(random()::text)::uuid, 'lynx-bap-0000-0000-0000-000000000000', 'lynx-cit-hels-inki-0000-000000000000',
    payment_type, payment_instrument, collected_by, priority, now(), now()
  FROM atlas_app.merchant_payment_method WHERE merchant_operating_city_id = v_base_city_id ON CONFLICT DO NOTHING;

  INSERT INTO atlas_app.merchant_payment_method (id, merchant_id, merchant_operating_city_id, payment_type, payment_instrument, collected_by, priority, updated_at, created_at)
  SELECT md5(random()::text)::uuid, 'bharat-t-bap0-0000-0000-000000000000', 'bharat-t-city-delh-0000-000000000000',
    payment_type, payment_instrument, collected_by, priority, now(), now()
  FROM atlas_app.merchant_payment_method WHERE merchant_operating_city_id = v_base_city_id ON CONFLICT DO NOTHING;
END $$;

-- exophone (required for confirm flow — virtual phone number for driver-rider masking)
INSERT INTO atlas_app.exophone (id, merchant_id, merchant_operating_city_id, primary_phone, backup_phone, is_primary_down, call_service, created_at, updated_at)
SELECT md5(random()::text)::uuid, m.id, moc.id, '+910000000001', '+910000000001', false, 'Exotel', now(), now()
FROM atlas_app.merchant_operating_city moc
JOIN atlas_app.merchant m ON m.id = moc.merchant_id
WHERE moc.id IN ('lynx-cit-hels-inki-0000-000000000000', 'bharat-t-city-delh-0000-000000000000')
ON CONFLICT DO NOTHING;
