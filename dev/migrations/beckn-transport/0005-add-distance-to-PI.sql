ALTER TABLE ONLY atlas_transporter.product_instance
ADD COLUMN IF NOT EXISTS distance double precision NOT NULL DEFAULT 0;
ALTER TABLE ONLY atlas_transporter.product_instance
ADD COLUMN IF NOT EXISTS dead_distance double precision NOT NULL DEFAULT 0;
