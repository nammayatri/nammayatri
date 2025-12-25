UPDATE atlas_driver_offer_bpp.transporter_config SET search_repeat_limit = 1;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN search_repeat_limit SET NOT NULL;
