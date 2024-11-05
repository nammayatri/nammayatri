ALTER TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details ADD COLUMN base_distance int;

ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section ADD COLUMN per_extra_km_round_trip_rate numeric(30, 2);

WITH PerExtraKmRate AS (
  SELECT T1.fare_policy_id,
    0,
    T1.per_km_rate_one_way,
    T1.per_km_rate_round_trip
  FROM atlas_driver_offer_bpp.fare_policy_inter_city_details AS T1
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details_per_extra_km_rate_section (
  fare_policy_id,
  start_distance,
  per_extra_km_rate,
  per_extra_km_round_trip_rate)
  (SELECT * FROM PerExtraKmRate);