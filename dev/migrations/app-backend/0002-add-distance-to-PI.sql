ALTER TABLE ONLY atlas_app.product_instance
ADD COLUMN IF NOT EXISTS actual_distance double precision;

ALTER TABLE ONLY atlas_app.product_instance
ADD COLUMN IF NOT EXISTS actual_price double precision;
