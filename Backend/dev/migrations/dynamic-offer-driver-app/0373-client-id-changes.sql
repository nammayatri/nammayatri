ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN actual_ride_distance_diff_threshold_if_within_pickup_drop double precision NOT NULL DEFAULT 2500;

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN client_id character varying(36);