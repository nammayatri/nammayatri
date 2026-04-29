-- Add gateTags column to gate_info table
ALTER TABLE atlas_driver_offer_bpp.gate_info ADD COLUMN gate_tags text[];
