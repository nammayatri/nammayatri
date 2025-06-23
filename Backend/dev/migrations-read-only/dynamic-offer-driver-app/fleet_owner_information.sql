CREATE TABLE atlas_driver_offer_bpp.fleet_owner_information ();

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN blocked boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN fleet_owner_person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN fleet_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN gst_number text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN verified boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD PRIMARY KEY ( fleet_owner_person_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN gst_image_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN referred_by_operator_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ALTER COLUMN referred_by_operator_id TYPE character varying (36);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN business_license_image_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN business_license_number text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN pan_number text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN pan_image_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN is_eligible_for_subscription boolean ;
