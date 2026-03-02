CREATE TABLE atlas_driver_offer_bpp.communication ();

ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN body text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN channels json ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN content_type character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN cta_button json ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN domain character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN html_body text ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN media_urls json ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN scheduled_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN sender_display_name character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN sender_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN sender_role character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN status character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN title character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN trigger_type character varying (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN variables json ;
ALTER TABLE atlas_driver_offer_bpp.communication ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN template_id character varying (255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.communication ADD COLUMN template_name character varying (255) ;