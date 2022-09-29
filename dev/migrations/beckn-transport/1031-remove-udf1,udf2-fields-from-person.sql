ALTER TABLE atlas_transporter.ride
  DROP CONSTRAINT ride_vehicle_id_fkey;

UPDATE atlas_transporter.vehicle AS v SET id = p.id
  FROM atlas_transporter.person AS p
    WHERE ( p.role = 'DRIVER' AND p.udf1 = v.id);

ALTER TABLE atlas_transporter.person DROP COLUMN IF EXISTS udf1;
ALTER TABLE atlas_transporter.person DROP COLUMN IF EXISTS udf2;

ALTER TABLE atlas_transporter.ride DROP COLUMN IF EXISTS vehicle_id;

ALTER TABLE atlas_transporter.vehicle RENAME COLUMN id TO driver_id;

ALTER TABLE atlas_transporter.vehicle
  ADD CONSTRAINT vehicle_driver_id_person_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_transporter.person(id);