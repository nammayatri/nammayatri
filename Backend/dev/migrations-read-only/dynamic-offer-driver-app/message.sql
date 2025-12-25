CREATE TABLE atlas_driver_offer_bpp.message ();

ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN message_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN always_trigger_on_onboarding boolean ;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN label character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN like_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN media_files text[] NOT NULL default ARRAY[]::TEXT[];
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN merchant_operating_city_id character varying(36) ;

ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN short_description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN title character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN view_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.message ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.message ADD COLUMN shareable boolean ;