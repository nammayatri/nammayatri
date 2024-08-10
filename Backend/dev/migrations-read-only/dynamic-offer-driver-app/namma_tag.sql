CREATE TABLE atlas_driver_offer_bpp.namma_tag ();

ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN chakra text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN event text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN tag_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN validity text ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN range_end double precision ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN range_start double precision ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN tags text[] ;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN rule text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag ADD PRIMARY KEY ( name);