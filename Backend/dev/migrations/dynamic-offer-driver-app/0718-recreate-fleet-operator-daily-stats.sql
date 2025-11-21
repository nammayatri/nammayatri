-- Don't run this in prod environment.
DROP TABLE IF EXISTS atlas_driver_offer_bpp.fleet_operator_daily_stats;

CREATE TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ();

ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN acceptation_request_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN customer_cancellation_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN driver_cancellation_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN driver_first_subscription integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN fleet_operator_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN inspection_completed integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN merchant_local_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN pulled_request_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN rejected_request_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN total_completed_rides integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN total_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN total_earning double precision ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN total_rating_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN total_rating_score integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN total_request_count integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_operator_daily_stats ADD PRIMARY KEY ( fleet_operator_id, merchant_local_date);

