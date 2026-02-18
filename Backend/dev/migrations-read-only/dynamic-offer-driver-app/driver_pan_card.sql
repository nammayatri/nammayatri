CREATE TABLE atlas_driver_offer_bpp.driver_pan_card ();

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN consent boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN consent_timestamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN document_image_id1 character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN document_image_id2 character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN driver_dob timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN driver_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN failed_rules text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN pan_card_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN pan_card_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN verified_by text ;
ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN merchant_operating_city_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN driver_name_on_govt_db text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN doc_type text ;



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_pan_card ADD COLUMN pan_aadhaar_linkage text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

