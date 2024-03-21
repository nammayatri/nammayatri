ALTER TABLE atlas_driver_offer_bpp.driver_location RENAME COLUMN updated_at to coordinates_calculated_at;
ALTER TABLE atlas_driver_offer_bpp.driver_location ALTER COLUMN coordinates_calculated_at DROP DEFAULT;
ALTER TABLE atlas_driver_offer_bpp.driver_location ADD COLUMN updated_at timestamp with time zone NOT NULL DEFAULT now();

WITH RideChargeableDistance AS (
  SELECT T1.id,
    CASE
      WHEN T1.traveled_distance > T2.estimated_distance
      THEN T1.traveled_distance
      ELSE T2.estimated_distance
    END AS chargeable_distance
  FROM atlas_driver_offer_bpp.ride AS T1
  JOIN atlas_driver_offer_bpp.booking AS T2
  ON T2.id = T1.booking_id
)
UPDATE atlas_driver_offer_bpp.ride AS T1
  SET chargeable_distance = (SELECT T2.chargeable_distance FROM RideChargeableDistance AS T2 WHERE T1.id = T2.id);
