CREATE TABLE atlas_driver_offer_bpp.vas_banner_config ();

ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN deep_link text ;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN image_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN link_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN priority integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN subtitle text ;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN valid_from timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN valid_to timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD COLUMN whatsapp_template_id text ;
ALTER TABLE atlas_driver_offer_bpp.vas_banner_config ADD PRIMARY KEY ( id);
