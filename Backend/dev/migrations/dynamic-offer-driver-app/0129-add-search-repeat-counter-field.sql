ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN search_repeat_counter int;
UPDATE atlas_driver_offer_bpp.search_request SET search_repeat_counter = 0;
ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN search_repeat_counter SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN search_repeat_limit int;
UPDATE atlas_driver_offer_bpp.transporter_config SET search_repeat_limit = 1;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN search_repeat_limit SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN transaction_id character(36);

ALTER TABLE atlas_driver_offer_bpp.booking ALTER COLUMN transaction_id SET NOT NULL;