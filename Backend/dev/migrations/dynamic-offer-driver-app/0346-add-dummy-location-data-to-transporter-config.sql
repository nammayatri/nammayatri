-- Add new column
ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN dummy_from_location json;

-- Add new column
ALTER TABLE atlas_driver_offer_bpp.transporter_config
ADD COLUMN dummy_to_location json;

-- Backfill the new columns
