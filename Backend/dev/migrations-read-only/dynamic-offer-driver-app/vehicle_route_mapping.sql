CREATE TABLE atlas_driver_offer_bpp.vehicle_route_mapping ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN allow_ending_mid_route boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN blocked boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_class text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_color text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_model text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_service_tier_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD PRIMARY KEY ( route_code, vehicle_number);
