ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN languages_to_be_translated Text[];

UPDATE atlas_driver_offer_bpp.transporter_config SET languages_to_be_translated = '{"HINDI","KANNADA","TAMIL","MALAYALAM","BENGALI","FRENCH","TELUGU"}';