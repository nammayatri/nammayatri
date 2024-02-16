ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN safety_alert_triggered boolean DEFAULT false;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN night_safety_checks boolean NOT NULL default true;

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN night_safety_route_deviation_threshold Int DEFAULT 1000 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN night_safety_start_time Int DEFAULT 75600 NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN night_safety_end_time Int DEFAULT 21600 NOT NULL;
