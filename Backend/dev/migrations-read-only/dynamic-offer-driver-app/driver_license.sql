CREATE TABLE atlas_driver_offer_bpp.driver_license ();

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN consent boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN consent_timestamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN document_image_id1 character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN document_image_id2 character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN driver_dob timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN driver_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN license_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN license_number text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN license_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN license_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license DROP COLUMN license_number;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN failed_rules text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN class_of_vehicles text[] NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN date_of_issue timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN reject_reason text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_license ADD COLUMN vehicle_category text ;