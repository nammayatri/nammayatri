CREATE TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ();

ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD COLUMN fleet_control_group_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD COLUMN fleet_member_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_control_group_member_association ADD PRIMARY KEY ( fleet_control_group_id, fleet_member_id);
