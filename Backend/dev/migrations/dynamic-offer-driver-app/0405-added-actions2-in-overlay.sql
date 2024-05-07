ALTER TABLE atlas_driver_offer_bpp.overlay ADD COLUMN actions2 json default json_build_array();
ALTER TABLE atlas_driver_offer_bpp.overlay ADD COLUMN secondary_actions2 json;
