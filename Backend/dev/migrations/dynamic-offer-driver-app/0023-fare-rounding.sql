-- ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_fare double precision;
-- UPDATE atlas_driver_offer_bpp.booking SET estimated_fare = 0;
UPDATE atlas_driver_offer_bpp.driver_quote SET estimated_fare = 0;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN estimated_fare SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
  ALTER COLUMN base_fare SET DATA TYPE integer
  USING round(base_fare);

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN base_distance TO base_distance_meters;


-- is the rounding necessary?
ALTER TABLE atlas_driver_offer_bpp.booking
  ALTER COLUMN estimated_distance SET DATA TYPE integer
  USING round(estimated_distance);

ALTER TABLE atlas_driver_offer_bpp.driver_quote
  ALTER COLUMN distance SET DATA TYPE integer
  USING round(distance);

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN base_fare SET DATA TYPE integer
  USING round(base_fare);

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN extra_km_fare SET DATA TYPE integer
  USING round(extra_km_fare);

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
  ALTER COLUMN driver_selected_fare SET DATA TYPE integer
  USING round(driver_selected_fare);

ALTER TABLE atlas_driver_offer_bpp.fare_policy RENAME COLUMN extra_km_fare TO per_extra_km_fare;

ALTER TABLE atlas_driver_offer_bpp.fare_policy
  ALTER COLUMN base_distance_meters SET DATA TYPE integer
  USING round(base_distance_meters);

ALTER TABLE atlas_driver_offer_bpp.fare_policy
  ALTER COLUMN dead_km_fare SET DATA TYPE integer
  USING round(dead_km_fare);

ALTER TABLE atlas_driver_offer_bpp.ride
  ALTER COLUMN fare SET DATA TYPE integer
  USING round(fare);

-- ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
--   ALTER COLUMN distance SET DATA TYPE integer
--   USING round(distance);

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN driver_min_extra_fee integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN driver_max_extra_fee integer;

WITH ExtraFeeList AS (
	SELECT id, unnest(driver_extra_fee_list) AS extraFee
	FROM   atlas_driver_offer_bpp.fare_policy
), MinMaxExtraFee AS (
	SELECT id, min (extraFee), max (extraFee)
	FROM ExtraFeeList
	GROUP BY id
)
UPDATE atlas_driver_offer_bpp.fare_policy AS T1 SET driver_min_extra_fee = T2.min, driver_max_extra_fee = T2.max
  FROM MinMaxExtraFee AS T2
  WHERE T1.id = T2.id;


ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN driver_extra_fee_list;
