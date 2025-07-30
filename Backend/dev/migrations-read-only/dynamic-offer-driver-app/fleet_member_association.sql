CREATE TABLE atlas_driver_offer_bpp.fleet_member_association ();

ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN fleet_member_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN group_code text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN is_fleet_owner boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN level integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN "order" integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN parent_group_code text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD PRIMARY KEY ( fleet_member_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_member_association DROP CONSTRAINT fleet_member_association_pkey;
ALTER TABLE atlas_driver_offer_bpp.fleet_member_association ADD PRIMARY KEY ( fleet_member_id, fleet_owner_id);