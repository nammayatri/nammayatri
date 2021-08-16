ALTER TABLE ONLY atlas_transporter.product_instance
ADD COLUMN IF NOT EXISTS actual_price double precision;
