CREATE TABLE atlas_driver_offer_bpp.message_translation ();

ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN label character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN message_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN short_description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD COLUMN title character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.message_translation ADD PRIMARY KEY ( message_id);