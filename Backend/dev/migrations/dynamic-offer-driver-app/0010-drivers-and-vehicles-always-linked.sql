UPDATE atlas_driver_offer_bpp.vehicle AS v SET driver_id = p.id
  FROM atlas_driver_offer_bpp.person AS p
    WHERE ( p.role = 'DRIVER' AND p.udf1 = v.driver_id);

ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN IF EXISTS udf1;
ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN IF EXISTS udf2;