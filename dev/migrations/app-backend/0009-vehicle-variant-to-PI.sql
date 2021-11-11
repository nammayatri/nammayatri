ALTER TABLE ONLY atlas_app.product_instance
  ADD COLUMN IF NOT EXISTS vehicle_variant character varying(60) NOT NULL DEFAULT '';
