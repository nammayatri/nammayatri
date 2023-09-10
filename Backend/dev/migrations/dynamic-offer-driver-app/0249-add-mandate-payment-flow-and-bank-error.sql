ALTER TABLE atlas_driver_offer_bpp.mandate ADD COLUMN mandate_payment_flow text;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN bank_error_message text;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN bank_error_code text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN bank_error_message text;
ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN bank_error_code text;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN bank_error_expiry bigint DEFAULT 3600 NOT NULL; -- error expires in 1 hour of updation

ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN payment_mode Text NOT NULL DEFAULT 'MANUAL_INVOICE';
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN bank_error_message Text;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN bank_error_code Text;
ALTER TABLE atlas_driver_offer_bpp.invoice ADD COLUMN bank_error_updated_at timestamp with time zone;
