ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN availability_time_window_option json NOT NULL DEFAULT '{"period":7, "periodType":"Days"}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN acceptance_ratio_window_option json NOT NULL DEFAULT '{"period":7, "periodType":"Days"}';
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_ratio_window_option json NOT NULL DEFAULT '{"period":7, "periodType":"Days"}';
