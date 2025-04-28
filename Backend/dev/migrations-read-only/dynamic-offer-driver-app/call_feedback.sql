CREATE TABLE atlas_driver_offer_bpp.call_feedback ();

ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN call_id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN entity_id character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN id character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN option_ids text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_feedback ADD PRIMARY KEY ( call_id, id);
