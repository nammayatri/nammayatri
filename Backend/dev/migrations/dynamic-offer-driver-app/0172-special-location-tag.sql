ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD column special_location_tag text;
ALTER TABLE atlas_driver_offer_bpp.estimate ADD column special_location_tag text;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD column special_location_tag text;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD column special_location_tag text;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD column "description" text;
CREATE TABLE atlas_driver_offer_bpp.special_location_priority (
  id character(36) NOT NULL PRIMARY KEY,
  merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
  category character varying(255) NOT NULL,
  pickup_priority integer NOT NULL,
  drop_priority integer NOT NULL
);
CREATE INDEX idx_special_location_priority ON atlas_driver_offer_bpp.special_location_priority USING btree (merchant_id, category);
-- All Category Priorities for different supported locations based on MerchantId
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'SureMetro',
      2,
      2
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'UnSureMetro',
      2,
      2
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'SureAirport',
      4,
      4
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'UnSureAirport',
      4,
      4
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'SureSchool',
      6,
      6
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'UnSureSchool',
      6,
      6
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'SureHospital',
      5,
      1
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'UnSureHospital',
      5,
      1
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'SureStation',
      3,
      3
  FROM atlas_driver_offer_bpp.merchant AS T1;
INSERT INTO atlas_driver_offer_bpp.special_location_priority (id, merchant_id, category, pickup_priority, drop_priority)
  SELECT
      md5(random()::text || clock_timestamp()::text)::uuid,
      T1.id,
      'UnSureStation',
      3,
      3
  FROM atlas_driver_offer_bpp.merchant AS T1;

-- Special Location Added For Chickpete Metro Station
INSERT INTO atlas_driver_offer_bpp.special_location (id, location_name, category, gates, geom, created_at)
    VALUES
    ( '9551c0b0-714f-4ec8-acbd-0719b90bc4bf'
    , 'Chickpete Metro Station'
    , 'SureMetro'
    , '{}'
    , '01060000000100000001030000000100000009000000500D66BDC86453405056D77F70EF2940F464AD57C6645340C8EA73926DEF294088F3A092BE6453406842A78BECEE2940C82E3C8EC76453404072D3C4CAEE2940301FE155C864534028FA71D2CEEE2940E48428F1C8645340706CBF40D6EE294084E2CC7CCC6453405C2EE35B15EF29404C923822CC645340C886D99F60EF2940500D66BDC86453405056D77F70EF2940'
    , now()
    );

CREATE TABLE atlas_driver_offer_bpp.fare_product (
  id character(36) NOT NULL PRIMARY KEY,
  merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy (id),
  vehicle_variant character varying(60) NOT NULL,
  "area" text NOT NULL,
  flow character varying(60) NOT NULL
);
CREATE INDEX idx_fare_product ON atlas_driver_offer_bpp.fare_product USING btree (merchant_id, vehicle_variant, "area");
-- NOTE :
-- FlowType should be Either All NORMAL or All RIDE_OTP for all VehicleVariants & Area of a Merchant.
-- However, for different Areas, we can have different FlowTypes but for all VehicleVariants it should be same. **

-- Fare Product Added For Chickpete Metro Station
-- Details : 4 Default Entries for 4 vehicle variants Per Merchant (Mandatory)
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.merchant_id,
        T1.id,
        T1.vehicle_variant,
        'Default',
        'NORMAL'
    FROM atlas_driver_offer_bpp.fare_policy AS T1;

-- SpecialLocation : 1 SpecialLocation x 2 PickupOrDrop x 4 VehicleVariants = Maximum of 8 Entries Per Merchant (Optional, Fallback is Default)
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Pickup_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'SUV' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Drop_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'SUV' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Pickup_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'SEDAN' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Drop_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'SEDAN' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Pickup_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'HATCHBACK' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Drop_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'HATCHBACK' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Pickup_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'AUTO_RICKSHAW' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';
-- INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", flow)
--     SELECT
--         md5(random()::text || clock_timestamp()::text)::uuid,
--         T1.merchant_id,
--         T1.id,
--         T1.vehicle_variant,
--         'Drop_9551c0b0-714f-4ec8-acbd-0719b90bc4bf',
--         'NORMAL'
--     FROM atlas_driver_offer_bpp.fare_policy AS T1 WHERE T1.vehicle_variant = 'AUTO_RICKSHAW' AND T1.merchant_id = 'favorit0-0000-0000-0000-00000favorit';

------------------------------------------------------------------------------------------------------------------------
--------------------------------------------DROP------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP column vehicle_variant;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP column merchant_id;