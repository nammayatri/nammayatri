CREATE TABLE atlas_driver_offer_bpp.entity_info ();

ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN answer text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN question text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN question_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.entity_info ADD PRIMARY KEY ( entity_id, entity_type, question_id);
