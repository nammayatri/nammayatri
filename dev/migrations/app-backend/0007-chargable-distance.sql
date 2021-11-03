CREATE TABLE atlas_app.product_instance_backup AS
  SELECT * from atlas_app.product_instance;

ALTER TABLE atlas_app.product_instance 
  DROP COLUMN actual_distance,
  ADD COLUMN chargable_distance double precision;