CREATE TABLE atlas_driver_offer_bpp.namma_tag_trigger ();

ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger ADD COLUMN event text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger ADD COLUMN tag_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger ADD PRIMARY KEY ( event, tag_name);
