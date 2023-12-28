ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN number_of_osrm_snap_to_road_calls Int;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD snap_to_road_confidence_threshold DOUBLE PRECISION NOT NULL DEFAULT 0.75;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD use_with_snap_to_road_fallback boolean NOT NULL DEFAULT true;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD snap_to_road_providers_list text[] NOT NULL DEFAULT '{"OSRM", "Google"}';