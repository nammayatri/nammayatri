ALTER TABLE atlas_app.payment_order ADD COLUMN IF NOT EXISTS pg_base_fee double precision;
ALTER TABLE atlas_app.payment_order ADD COLUMN IF NOT EXISTS pg_gst double precision;
