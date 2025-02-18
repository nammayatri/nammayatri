CREATE TABLE atlas_driver_offer_bpp.fleet_route_association ();

ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_route_association ADD PRIMARY KEY ( id);
