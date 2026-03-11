CREATE TABLE atlas_driver_offer_bpp.message_dictionary ();

ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN message_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN message_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.message_dictionary ADD PRIMARY KEY ( id);
