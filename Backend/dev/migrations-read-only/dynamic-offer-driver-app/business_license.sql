CREATE TABLE atlas_driver_offer_bpp.business_license ();

ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN document_image_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN license_expiry timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN license_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN license_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN verification_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.business_license ADD PRIMARY KEY ( id);
