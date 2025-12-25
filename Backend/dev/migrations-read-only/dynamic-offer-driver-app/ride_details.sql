CREATE TABLE atlas_driver_offer_bpp.ride_details ();

ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN default_service_tier_name text ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN driver_country_code character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN driver_name character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN driver_number_encrypted character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN driver_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN fleet_owner_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN vehicle_class character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN vehicle_color character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN vehicle_model character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN vehicle_number character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN vehicle_variant character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN vehicle_age int ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

CREATE INDEX ride_details_idx_vehicle_number ON atlas_driver_offer_bpp.ride_details USING btree (vehicle_number);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.ride_details ADD COLUMN rc_id character varying(36) ;