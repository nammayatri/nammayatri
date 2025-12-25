CREATE TABLE atlas_driver_offer_bpp.translations ();

ALTER TABLE atlas_driver_offer_bpp.translations ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.translations ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.translations ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.translations ADD COLUMN message text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.translations ADD COLUMN message_key text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.translations ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.translations ADD PRIMARY KEY ( id, language, message_key);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.translations DROP CONSTRAINT translations_pkey;
ALTER TABLE atlas_driver_offer_bpp.translations ADD PRIMARY KEY ( id);