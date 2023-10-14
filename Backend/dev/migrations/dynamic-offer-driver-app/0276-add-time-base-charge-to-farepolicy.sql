ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS ride_extra_time_fare int;
ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN IF NOT EXISTS per_minute_ride_extra_time_charge int; -- make this double precision

WITH suv_fp_ids AS (
    SELECT fare_policy_id
    FROM atlas_driver_offer_bpp.fare_product
    WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'
    AND vehicle_variant = 'SUV'
)
UPDATE atlas_driver_offer_bpp.fare_policy SET per_minute_ride_extra_time_charge=2
WHERE id IN (SELECT fare_policy_id FROM suv_fp_ids);

WITH sedan_fp_ids AS (
    SELECT fare_policy_id
    FROM atlas_driver_offer_bpp.fare_product
    WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'
    AND vehicle_variant = 'SEDAN'
)
UPDATE atlas_driver_offer_bpp.fare_policy SET per_minute_ride_extra_time_charge=1.5
WHERE id IN (SELECT fare_policy_id FROM sedan_fp_ids);