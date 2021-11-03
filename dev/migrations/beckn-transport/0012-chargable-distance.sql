ALTER TABLE atlas_transporter.product_instance
  RENAME COLUMN distance TO traveled_distance;
  
ALTER TABLE atlas_transporter.product_instance
  ADD COLUMN chargable_distance double precision;