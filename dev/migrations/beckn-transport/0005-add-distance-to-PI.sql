ALTER TABLE ONLY atlas_transporter.product_instance
ADD COLUMN IF NOT EXISTS distance double precision NOT NULL DEFAULT 0;
