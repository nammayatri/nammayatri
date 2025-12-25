CREATE TABLE atlas_driver_offer_bpp.call_feedback_options ();

ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN category character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN message_key character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN merchant_id character varying (36) ;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN merchant_operating_city_id character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.call_feedback_options ADD PRIMARY KEY ( id);
