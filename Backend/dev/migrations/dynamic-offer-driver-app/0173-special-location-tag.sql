ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD column special_location_tag text;
ALTER TABLE atlas_driver_offer_bpp.search_request_special_zone ADD column "area" text;
--ALTER TABLE atlas_driver_offer_bpp.booking ADD column "area" text;
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

CREATE INDEX idx_fare_product ON atlas_driver_offer_bpp.fare_product USING btree (merchant_id, vehicle_variant, "area");
-- NOTE :
-- FlowType should be Either All NORMAL or All RIDE_OTP for all VehicleVariants & Area of a Merchant.
-- However, for different Areas, we can have different FlowTypes but for all VehicleVariants it should be same. **

-- Default Fare Product with Normal Estimates For All Merchants
-- Details : 4 Default Entries for 4 vehicle variants Per Merchant (Mandatory)
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", enabled, merchant_operating_city_id, time_bounds, trip_category)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T1.merchant_id,
        T1.id,
        T1.vehicle_variant,
        'Default',
        true,
        'namma-yatri-0-0000-0000-00000000city',
        'Unbounded',
        'OneWay_OneWayOnDemandDynamicOffer'
    FROM atlas_driver_offer_bpp.fare_policy AS T1;

-- Special Location Fare Product with RideOTP Quotes For All Merchants
-- SpecialLocation : 1 SpecialLocation x 4 VehicleVariants x 2 PickupOrDrop = Maximum of 8 Entries Per Merchant (Optional, Fallback is Default but it will result in 2 DB Queries rather then One. So its good to have link)
CREATE TEMPORARY TABLE pickup_drop_table (
  pickup_drop_type text
);
INSERT INTO pickup_drop_table (pickup_drop_type) VALUES ('Pickup'), ('Drop');
INSERT INTO atlas_driver_offer_bpp.fare_product (id, merchant_id, fare_policy_id, vehicle_variant, "area", enabled, merchant_operating_city_id, time_bounds, trip_category)
    SELECT
        md5(random()::text || clock_timestamp()::text)::uuid,
        T2.merchant_id,
        T2.id,
        T2.vehicle_variant,
        concat(T3.pickup_drop_type, '_', T1.id),
        true,
        'namma-yatri-0-0000-0000-00000000city',
        'Unbounded',
        'OneWay_OneWayRideOtp'
    FROM atlas_driver_offer_bpp.special_location AS T1
    CROSS JOIN atlas_driver_offer_bpp.fare_policy AS T2
    CROSS JOIN pickup_drop_table AS T3;
    -- where merchant_id='...' -- can be merchant specific, for NY we may want Normal Estimates but for JaatriSaathi we may need Special Zone Quotes

------------------------------------------------------------------------------------------------------------------------
--------------------------------------------DROP------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP column vehicle_variant;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP column merchant_id;