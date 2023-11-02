
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN is_retried boolean DEFAULT false NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN is_retargeted boolean DEFAULT false NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_order ADD COLUMN retarget_link Text;

ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN response_code Text;
ALTER TABLE atlas_driver_offer_bpp.notification ADD COLUMN response_message Text;