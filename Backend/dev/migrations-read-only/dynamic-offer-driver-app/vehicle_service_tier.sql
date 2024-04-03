CREATE TABLE atlas_driver_offer_bpp.vehicle_service_tier ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN air_conditioned double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN allowed_vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN driver_rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN long_description text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN luggage_capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN seating_capacity integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN short_description text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN vehicle_rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN service_tier_type text NOT NULL;