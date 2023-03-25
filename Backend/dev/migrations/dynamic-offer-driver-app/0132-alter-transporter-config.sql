ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN default_popup_delay int;
UPDATE atlas_driver_offer_bpp.transporter_config SET default_popup_delay = 2;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ALTER COLUMN default_popup_delay SET NOT NULL;