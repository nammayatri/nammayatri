ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN driver_location_accuracy_buffer Int DEFAULT 10 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN route_deviation_threshold Int DEFAULT 50 NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN driver_deviated_from_route boolean;
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_snap_to_road_calls Int;
