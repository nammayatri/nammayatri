CREATE TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ();

ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN merchant_local_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN rc_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN ride_distance integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN ride_duration integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN total_completed_rides integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN total_earnings double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_daily_stats ADD PRIMARY KEY ( fleet_owner_id, merchant_local_date, rc_id);


