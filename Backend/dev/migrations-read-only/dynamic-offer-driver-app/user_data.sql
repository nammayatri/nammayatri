CREATE TABLE atlas_driver_offer_bpp.user_data ();

ALTER TABLE atlas_driver_offer_bpp.user_data ADD COLUMN chakra text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_data ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_data ADD COLUMN user_data_value text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_data ADD COLUMN user_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.user_data ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.user_data ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.user_data ADD PRIMARY KEY ( id);