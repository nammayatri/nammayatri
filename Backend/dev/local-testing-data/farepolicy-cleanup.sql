-- FarePolicy test cleanup: keep exactly ONE fare product per
-- (merchant_operating_city_id, vehicle_variant, trip_category) for Bangalore.
-- Run AFTER config-sync and AFTER driver-app restart.

-- Step 1: For each (mocId, variant, tripCategory), keep only the Unbounded/Default
-- fare product and delete the rest. This ensures the search always resolves to
-- exactly one policy.
WITH bangalore AS (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city WHERE city = 'Bangalore'
),
keepers AS (
  SELECT DISTINCT ON (fp.merchant_operating_city_id, fp.vehicle_variant, fp.trip_category)
         fp.id
  FROM atlas_driver_offer_bpp.fare_product fp
  JOIN bangalore b ON fp.merchant_operating_city_id = b.id
  ORDER BY fp.merchant_operating_city_id, fp.vehicle_variant, fp.trip_category,
           -- prefer Default area, then Unbounded timeBounds
           CASE WHEN fp.area = 'Default' THEN 0 ELSE 1 END,
           CASE WHEN fp.time_bounds = 'Unbounded' THEN 0 ELSE 1 END,
           fp.id
)
DELETE FROM atlas_driver_offer_bpp.fare_product fp
USING bangalore b
WHERE fp.merchant_operating_city_id = b.id
  AND fp.id NOT IN (SELECT id FROM keepers);

-- Step 2: Fix NULL ids in driver_extra_fee_bounds
UPDATE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds
SET id = sub.rn
FROM (
  SELECT ctid, ROW_NUMBER() OVER (PARTITION BY fare_policy_id ORDER BY start_distance) - 1 AS rn
  FROM atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds WHERE id IS NULL
) sub
WHERE fare_policy_driver_extra_fee_bounds.ctid = sub.ctid;

-- Step 3: Remove fare_products with missing or broken fare_policy data
-- 3a: Missing fare_policy row entirely
DELETE FROM atlas_driver_offer_bpp.fare_product fp
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.fare_policy p WHERE p.id = fp.fare_policy_id
);

-- 3b: Progressive policies missing per_extra_km_rate_section
DELETE FROM atlas_driver_offer_bpp.fare_product
WHERE fare_policy_id IN (
  SELECT fp.id FROM atlas_driver_offer_bpp.fare_policy fp
  JOIN atlas_driver_offer_bpp.fare_policy_progressive_details fpd ON fp.id = fpd.fare_policy_id
  WHERE fp.fare_policy_type = 'Progressive'
    AND NOT EXISTS (
      SELECT 1 FROM atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section fprs
      WHERE fprs.fare_policy_id = fp.id
    )
);

-- 3c: Rental policies missing rental_details
DELETE FROM atlas_driver_offer_bpp.fare_product
WHERE fare_policy_id IN (
  SELECT fp.id FROM atlas_driver_offer_bpp.fare_policy fp
  WHERE fp.fare_policy_type = 'Rental'
    AND NOT EXISTS (
      SELECT 1 FROM atlas_driver_offer_bpp.fare_policy_rental_details rd
      WHERE rd.fare_policy_id = fp.id
    )
);

-- 3d: InterCity policies missing inter_city_details
DELETE FROM atlas_driver_offer_bpp.fare_product
WHERE fare_policy_id IN (
  SELECT fp.id FROM atlas_driver_offer_bpp.fare_policy fp
  WHERE fp.fare_policy_type = 'InterCity'
    AND NOT EXISTS (
      SELECT 1 FROM atlas_driver_offer_bpp.fare_policy_inter_city_details icd
      WHERE icd.fare_policy_id = fp.id
    )
);

-- Step 4: Disable KV for fare policy tables so replace writes directly to Postgres
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = jsonb_set(
    config_value::jsonb, '{disableForKV}',
    (config_value::jsonb -> 'disableForKV') || (
      SELECT COALESCE(jsonb_agg(t), '[]'::jsonb)
      FROM (VALUES ('fare_policy'),('fare_product'),
        ('fare_policy_progressive_details'),('fare_policy_rental_details'),
        ('fare_policy_inter_city_details'),('fare_policy_ambulance_details_slab'),
        ('fare_policy_slabs_details_slab'),('fare_policy_driver_extra_fee_bounds'),
        ('fare_policy_progressive_details_per_extra_km_rate_section'),
        ('fare_policy_rental_details_distance_buffers'),
        ('fare_policy_rental_details_pricing_slabs'),
        ('fare_policy_inter_city_details_pricing_slabs')
      ) AS missing(t)
      WHERE NOT (config_value::jsonb -> 'disableForKV') @> to_jsonb(t)
    ))::text
WHERE id = 'kv_configs';

-- Step 5: Fix vehicle_age column type (config-sync imports as text, Beam expects integer)
DO $$ BEGIN
  ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab
  ALTER COLUMN vehicle_age TYPE integer USING vehicle_age::integer;
EXCEPTION WHEN OTHERS THEN NULL;
END $$;

-- Step 6: Remove leftover test Ambulance fare products (created by Phase 4)
DELETE FROM atlas_driver_offer_bpp.fare_product fp
USING atlas_driver_offer_bpp.merchant_operating_city moc
WHERE fp.merchant_operating_city_id = moc.id AND moc.city = 'Bangalore'
  AND fp.trip_category = 'Ambulance_OneWayOnDemandDynamicOffer';

-- Step 7: Remove leftover test Slabs fare products (created by Phase 5)
DELETE FROM atlas_driver_offer_bpp.fare_product fp
USING atlas_driver_offer_bpp.merchant_operating_city moc,
      atlas_driver_offer_bpp.fare_policy p
WHERE fp.merchant_operating_city_id = moc.id AND moc.city = 'Bangalore'
  AND fp.fare_policy_id = p.id AND p.fare_policy_type = 'Slabs'
  AND fp.search_source = 'DASHBOARD';

-- Step 8: Seed Ambulance document_verification_config for Bangalore (needed for addVehicle)
INSERT INTO atlas_driver_offer_bpp.document_verification_config
  (document_type, vehicle_category, merchant_id, merchant_operating_city_id,
   check_expiry, check_extraction, dependency_document_type, is_disabled, is_hidden, is_mandatory, max_retry_count,
   title, vehicle_class_check_type, created_at, updated_at, "order",
   is_default_enabled_on_manual_verification, is_image_validation_required,
   do_strict_verifcation, filter_for_old_apks, supported_vehicle_classes_json,
   applicable_to, document_flow_grouping, rc_number_prefix_list)
VALUES
  ('VehicleRegistrationCertificate', 'AMBULANCE', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98',
   false, true, '{}', false, false, true, 4,
   'Vehicle Registration Certificate', 'Infix', now(), now(), 1,
   false, true, true, false,
   '[{"bodyType": null, "priority": 1, "manufacturer": null, "vehicleClass": "Ambulance", "vehicleVariant": "AMBULANCE_TAXI_OXY", "vehicleCapacity": null}]',
   'FLEET_AND_INDIVIDUAL', 'STANDARD', '{}')
ON CONFLICT DO NOTHING;

-- Step 8: Seed an Ambulance driver for Bangalore (needed for search estimates)
-- Insert person (driver)
INSERT INTO atlas_driver_offer_bpp.person
  (id, first_name, gender, identifier_type, role, merchant_id, merchant_operating_city_id,
   created_at, updated_at, onboarded_from_dashboard, is_new, total_earned_coins, used_coins)
VALUES
  ('amb-test-driver-00000000-0000-0001', 'AmbTestDriver', 'MALE', 'MOBILENUMBER', 'DRIVER',
   '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98',
   now(), now(), true, false, 0, 0)
ON CONFLICT (id) DO NOTHING;

-- Insert vehicle
INSERT INTO atlas_driver_offer_bpp.vehicle
  (driver_id, merchant_id, variant, model, color, vehicle_class, category, make, registration_no, capacity, created_at, updated_at)
VALUES
  ('amb-test-driver-00000000-0000-0001', '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'AMBULANCE_TAXI_OXY', 'AMBULANCE TAXI OXY', 'White', 'Ambulance', 'AMBULANCE', 'Tata', 'KA00AMB0001', 4, now(), now())
ON CONFLICT (driver_id) DO NOTHING;

-- Insert driver_information
INSERT INTO atlas_driver_offer_bpp.driver_information
  (driver_id, active, on_ride, enabled, verified, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES
  ('amb-test-driver-00000000-0000-0001', true, false, true, true, '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', '1e7b7ab9-3b9b-4d3e-a47c-11e7d2a9ff98', now(), now())
ON CONFLICT (driver_id) DO NOTHING;

-- Insert driver_location so search can find this driver
INSERT INTO atlas_driver_offer_bpp.driver_location
  (driver_id, lat, lon, point, merchant_id, created_at, updated_at, coordinates_calculated_at)
VALUES
  ('amb-test-driver-00000000-0000-0001', 12.9352723, 77.6244867, ST_SetSRID(ST_Point(77.6244867, 12.9352723), 4326),
   '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', now(), now(), now())
ON CONFLICT (driver_id) DO UPDATE SET lat = 12.9352723, lon = 77.6244867, updated_at = now(), coordinates_calculated_at = now();

-- Step 9: Fix rider-app merchant NULL kapture_disposition
UPDATE atlas_app.merchant SET kapture_disposition = '' WHERE kapture_disposition IS NULL;

-- Step 10: Enable Ambulance VST for Namma Yatri Bangalore
-- The seed entry has is_enabled='f' and wrong vehicle variants, preventing ambulance estimates.
UPDATE atlas_driver_offer_bpp.vehicle_service_tier
SET is_enabled = true,
    allowed_vehicle_variant = '{AMBULANCE_TAXI_OXY}',
    default_for_vehicle_variant = '{AMBULANCE_TAXI_OXY}',
    auto_selected_vehicle_variant = '{AMBULANCE_TAXI_OXY}',
    name = 'Basic Support - Mini',
    short_description = 'Airway management',
    oxygen = 1,
    priority = 0,
    seating_capacity = NULL,
    updated_at = now()
WHERE id = 'b2c0e8c8-08ff-5fba-a76b-a00d8841a26d';

-- Step 11: Set Bangalore city ID on BAP ambulance beckn_config
UPDATE atlas_app.beckn_config
SET merchant_operating_city_id = '96327a06-c7d9-460e-8bad-c6a3811915fb'
WHERE id = '6f02ef68-693a-e267-b926-c2cfab70436e'
  AND vehicle_category = 'AMBULANCE'
  AND merchant_operating_city_id IS NULL;
