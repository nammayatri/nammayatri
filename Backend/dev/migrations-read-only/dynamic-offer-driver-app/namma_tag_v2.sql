CREATE TABLE atlas_driver_offer_bpp.namma_tag_v2 ();

ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN action_engine json ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN chakra text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN tag_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN range_end double precision ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN range_start double precision ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN tags text[] ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN llm_context text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN rule_engine json ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN validity integer ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_v2 ADD PRIMARY KEY ( merchant_operating_city_id, name);
