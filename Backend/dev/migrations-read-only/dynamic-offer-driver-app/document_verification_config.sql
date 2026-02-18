CREATE TABLE atlas_driver_offer_bpp.document_verification_config ();

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN check_expiry boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN check_extraction boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN dependency_document_type text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN disable_warning text ;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN is_disabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN is_hidden boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN is_mandatory boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN max_retry_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN rc_number_prefix_list text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN supported_vehicle_classes_json json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN vehicle_class_check_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD PRIMARY KEY ( document_type, merchant_operating_city_id, vehicle_category);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN "order" integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN dl_number_verification boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config DROP COLUMN dl_number_verification;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN is_default_enabled_on_manual_verification boolean NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ALTER COLUMN is_default_enabled_on_manual_verification SET DEFAULT true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN is_image_validation_required boolean NOT NULL default true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN do_strict_verifcation boolean NOT NULL default true;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN filter_for_old_apks boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN role text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN document_category text ;



------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN is_mandatory_for_enabling boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN document_fields_json json ;
ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN applicable_to text  default 'FLEET_AND_INDIVIDUAL';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN allow_license_transfer boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.document_verification_config ADD COLUMN document_flow_grouping text  default 'STANDARD';



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

