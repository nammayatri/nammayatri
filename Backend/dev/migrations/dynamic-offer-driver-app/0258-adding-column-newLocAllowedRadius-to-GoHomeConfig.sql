ALTER TABLE atlas_driver_offer_bpp.go_home_config ADD COLUMN new_loc_allowed_radius integer NOT NULL DEFAULT 20;
UPDATE atlas_driver_offer_bpp.go_home_config SET new_loc_allowed_radius = 20;