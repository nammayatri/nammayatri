ALTER TABLE ONLY atlas_transporter.product_instance
  ADD COLUMN IF NOT EXISTS vehicle_variant character varying(60);

UPDATE atlas_transporter.product_instance
  SET vehicle_variant =
    ( SELECT COALESCE(variant, 'SUV')
        FROM atlas_transporter.vehicle
        WHERE atlas_transporter.vehicle.id = atlas_transporter.product_instance.entity_id
    );

UPDATE atlas_transporter.product_instance
  SET price = 0
  WHERE price IS NULL;

ALTER TABLE atlas_transporter.product_instance
  ALTER COLUMN price SET NOT NULL;

ALTER TABLE atlas_transporter.product_instance
  ALTER COLUMN vehicle_variant SET NOT NULL;
