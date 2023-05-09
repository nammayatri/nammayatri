CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  start_distance integer NOT NULL,
  per_extra_km_rate numeric (30,2) NOT NULL,
  CONSTRAINT fare_policy_progressive_details_per_extra_km_rate_section_unique_start_distance UNIQUE (fare_policy_id, start_distance)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section OWNER TO atlas_driver_offer_bpp_user;

WITH PerExtraKmRate AS (
  SELECT T1.fare_policy_id,
    0,
    T1.per_extra_km_fare
  FROM atlas_driver_offer_bpp.fare_policy_progressive_details AS T1
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
  fare_policy_id,
  start_distance,
  per_extra_km_rate)
  (SELECT * FROM PerExtraKmRate);

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ALTER COLUMN per_extra_km_fare DROP NOT NULL;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details DROP COLUMN per_extra_km_fare;