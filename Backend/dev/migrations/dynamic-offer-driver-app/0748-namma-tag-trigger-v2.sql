CREATE TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ();

ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ADD COLUMN event text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ADD COLUMN tag_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.namma_tag_trigger_v2 ADD PRIMARY KEY (merchant_operating_city_id, event, tag_name);
