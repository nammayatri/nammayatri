CREATE TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ();

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN check_expiry boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN check_extraction boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN dependency_document_type text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN disable_warning text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN document_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN is_disabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN is_hidden boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN is_mandatory boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN max_retry_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN "order" integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD PRIMARY KEY ( document_type, merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN is_image_validation_required boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN is_default_enabled_on_manual_verification boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN do_strict_verifcation boolean NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN role text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD COLUMN document_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ALTER COLUMN role SET DEFAULT 'FLEET_OWNER';
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ALTER COLUMN role SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config DROP CONSTRAINT fleet_owner_document_verification_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.fleet_owner_document_verification_config ADD PRIMARY KEY ( document_type, merchant_operating_city_id, role);
