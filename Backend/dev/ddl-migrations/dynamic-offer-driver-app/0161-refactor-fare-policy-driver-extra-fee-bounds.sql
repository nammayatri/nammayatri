CREATE TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  start_distance integer NOT NULL,
  min_fee integer NOT NULL,
  max_fee integer NOT NULL,
  CONSTRAINT fare_policy_driver_extra_fee_bounds_unique_start_distance UNIQUE (fare_policy_id, start_distance)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_driver_extra_fee_bounds OWNER TO atlas_driver_offer_bpp_user;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN driver_min_extra_fee;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN driver_max_extra_fee;

DROP TABLE atlas_driver_offer_bpp.restricted_extra_fare;
