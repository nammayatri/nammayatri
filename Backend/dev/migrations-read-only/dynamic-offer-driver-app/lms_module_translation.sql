CREATE TABLE atlas_driver_offer_bpp.lms_module_translation ();

ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN module_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN thumbnail_image text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_module_translation ADD PRIMARY KEY ( language, module_id);