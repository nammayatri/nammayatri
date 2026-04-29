ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN bank_error_message text;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN bank_error_code text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN bank_error_message text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN bank_error_code text;