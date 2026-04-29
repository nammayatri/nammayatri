ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN IF NOT EXISTS pg_base_fee double precision;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN IF NOT EXISTS pg_gst double precision;
