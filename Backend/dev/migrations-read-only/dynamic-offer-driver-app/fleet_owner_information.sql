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


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN aadhaar_number text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN aadhaar_front_image_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN aadhaar_back_image_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN registered_at timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN pan_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN pan_number_encrypted character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN gst_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN gst_number_encrypted character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN business_license_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN business_license_number_encrypted character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN aadhaar_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN aadhaar_number_encrypted character varying (255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ALTER COLUMN referred_by_operator_id TYPE text;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_information ADD COLUMN is_eligible_for_subscription boolean ;