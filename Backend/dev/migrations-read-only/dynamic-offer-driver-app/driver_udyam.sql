CREATE TABLE atlas_driver_offer_bpp.driver_udyam ();

ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN enterprise_name text ;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN enterprise_type text ;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN udyam_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN udyam_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN verified_by text ;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_udyam ADD PRIMARY KEY ( id);
