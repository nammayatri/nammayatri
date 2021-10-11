ALTER TABLE ONLY atlas_transporter.product_instance
  ADD COLUMN IF NOT EXISTS vehicle_variant character varying(60);

UPDATE atlas_transporter.product_instance
  SET vehicle_variant =
    ( SELECT COALESCE(atlas_transporter."case".udf1, 'SUV')
        FROM atlas_transporter."case"
        WHERE atlas_transporter."case".id = atlas_transporter.product_instance.case_id
    );

UPDATE atlas_transporter.product_instance
  SET price = 0
  WHERE price IS NULL;

ALTER TABLE atlas_transporter.product_instance
  ALTER COLUMN price SET NOT NULL;

ALTER TABLE atlas_transporter.product_instance
  ALTER COLUMN vehicle_variant SET NOT NULL;
