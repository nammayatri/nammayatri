CREATE TABLE atlas_driver_offer_bpp.idfy_verification ();

ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN dashboard_passed_vehicle_variant text ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN doc_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_image_id1 character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_image_id2 character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN document_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN driver_date_of_birth timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN idfy_response text ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN image_extraction_validation text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN issue_date_on_doc timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN multiple_rc boolean ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN request_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN retry_count integer ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD PRIMARY KEY ( id, driver_id, request_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN name_on_card text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.idfy_verification DROP CONSTRAINT idfy_verification_pkey;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN vehicle_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.idfy_verification DROP COLUMN dashboard_passed_vehicle_variant;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN ventilator boolean ;
ALTER TABLE atlas_driver_offer_bpp.idfy_verification ADD COLUMN oxygen boolean ;



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

