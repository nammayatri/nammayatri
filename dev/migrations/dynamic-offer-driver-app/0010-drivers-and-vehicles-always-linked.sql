UPDATE atlas_driver_offer_bpp.vehicle AS v SET id = p.id
  FROM atlas_driver_offer_bpp.person AS p
    WHERE ( p.role = 'DRIVER' AND p.udf1 = v.id);

ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN IF EXISTS udf1;
ALTER TABLE atlas_driver_offer_bpp.person DROP COLUMN IF EXISTS udf2;
ALTER TABLE atlas_driver_offer_bpp.ride DROP COLUMN IF EXISTS vehicle_id;
ALTER TABLE atlas_driver_offer_bpp.vehicle RENAME COLUMN id TO driver_id;
ALTER TABLE atlas_driver_offer_bpp.vehicle
  ADD CONSTRAINT vehicle_driver_id_person_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id);
