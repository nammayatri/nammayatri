ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN max_radius bigint DEFAULT 1500 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN min_radius bigint DEFAULT 700 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN radius_step_size bigint DEFAULT 500 NOT NULL;
