ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN IF NOT EXISTS paytm_tid_encrypted text;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN IF NOT EXISTS paytm_tid_hash bytea;
