CREATE TABLE atlas_driver_offer_bpp.fleet_control_group ();

ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN fleet_control_group_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN fleet_control_group_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group ADD PRIMARY KEY ( fleet_control_group_id);
