CREATE TABLE atlas_driver_offer_bpp.message_report ();

ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN delivery_status character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN like_status boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN message_dynamic_fields json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN message_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN read_status boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN reply text ;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.message_report ADD PRIMARY KEY ( driver_id, message_id);