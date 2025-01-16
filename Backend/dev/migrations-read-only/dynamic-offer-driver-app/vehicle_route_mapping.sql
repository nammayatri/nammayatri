CREATE TABLE atlas_driver_offer_bpp.vehicle_route_mapping ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN blocked boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN vehicle_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_route_mapping ADD PRIMARY KEY ( route_code, vehicle_number_hash);
