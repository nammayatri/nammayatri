CREATE TABLE atlas_driver_offer_bpp.fleet_rc_association ();

ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN associated_on timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN associated_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN is_rc_active boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN rc_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN rc_verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ADD COLUMN vehicle_registration_no text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association ALTER COLUMN vehicle_registration_no DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association DROP COLUMN is_rc_active;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association DROP COLUMN driver_id;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association DROP COLUMN vehicle_registration_no;
ALTER TABLE atlas_driver_offer_bpp.fleet_rc_association DROP COLUMN rc_verification_status;