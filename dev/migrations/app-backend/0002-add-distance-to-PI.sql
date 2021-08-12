ALTER TABLE ONLY atlas_app.product_instance
ADD COLUMN IF NOT EXISTS distance double precision NOT NULL DEFAULT 0;
