ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN actual_ride_distance_diff_threshold double precision NOT NULL DEFAULT 1200;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN upwards_recompute_buffer double precision NOT NULL DEFAULT 2000;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN approx_ride_distance_diff_threshold double precision NOT NULL DEFAULT 1200;
