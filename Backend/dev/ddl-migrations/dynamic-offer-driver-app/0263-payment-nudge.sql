CREATE INDEX idx_merchant_overlay_key ON atlas_driver_offer_bpp.merchant_overlay (merchant_id,overlay_key);
-- PLEASE DON'T RUN THE NEXT LINE (ONLY FOR LOCAL)
ALTER TABLE atlas_driver_offer_bpp.merchant_overlay ALTER COLUMN merchant_operating_city_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.payment_order ALTER COLUMN amount type numeric(30,2);
