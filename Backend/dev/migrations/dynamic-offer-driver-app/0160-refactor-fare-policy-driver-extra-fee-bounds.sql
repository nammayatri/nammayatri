CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  start_distance integer NOT NULL,
  min_fee integer NOT NULL,
  max_fee integer NOT NULL,
  CONSTRAINT fare_policy_driver_extra_fee_bounds_unique_start_distance UNIQUE (fare_policy_id, start_distance)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds OWNER TO atlas_driver_offer_bpp_user;

WITH ExtraFeeBounds1 AS (
  SELECT T1.id,
    0,
    T1.driver_min_extra_fee,
    T1.driver_max_extra_fee
  FROM atlas_driver_offer_bpp.fare_policy AS T1
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
  fare_policy_id,
  start_distance,
  min_fee,
  max_fee)
  (SELECT * FROM ExtraFeeBounds1);

WITH ExtraFeeBounds2 AS (
  SELECT T2.id,
    T1.min_trip_distance,
    T2.driver_min_extra_fee,
    T1.driver_max_extra_fare
  FROM atlas_driver_offer_bpp.restricted_extra_fare AS T1
  JOIN atlas_driver_offer_bpp.fare_policy AS T2
    ON T1.vehicle_variant = T2.vehicle_variant AND T1.merchant_id = T2.merchant_id
  WHERE T1.min_trip_distance <> 0
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
  fare_policy_id,
  start_distance,
  min_fee,
  max_fee)
  (SELECT * FROM ExtraFeeBounds2);

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN driver_min_extra_fee;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN driver_max_extra_fee;

DROP TABLE atlas_driver_offer_bpp.restricted_extra_fare;
