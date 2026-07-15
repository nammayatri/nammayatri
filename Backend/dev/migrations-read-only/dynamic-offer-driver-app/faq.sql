CREATE TABLE atlas_driver_offer_bpp.faq ();

ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN answer text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN category character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN faq_group_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN question text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.faq ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.faq ADD PRIMARY KEY ( id);
