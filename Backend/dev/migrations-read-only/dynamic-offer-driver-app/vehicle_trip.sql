CREATE TABLE atlas_driver_offer_bpp.vehicle_trip ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN end_reason text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN ended_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN started_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN vehicle_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN walkup_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN to_stop text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN from_stop text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_trip ADD COLUMN earnings double precision ;