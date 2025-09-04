CREATE TABLE atlas_driver_offer_bpp.fleet_config ();

ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN allow_automatic_round_trip_assignment boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN allow_ending_mid_route boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN allow_start_ride_from_qr boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN end_ride_distance_threshold double precision  default 100;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN ride_end_approval boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD PRIMARY KEY ( fleet_owner_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN directly_start_first_trip_assignment boolean  default true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN unlink_driver_and_vehicle_on_trip_termination boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_config ALTER COLUMN unlink_driver_and_vehicle_on_trip_termination SET DEFAULT true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_config ADD COLUMN blacklist_coin_events text[]  default '{}';