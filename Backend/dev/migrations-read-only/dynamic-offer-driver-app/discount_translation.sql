CREATE TABLE atlas_driver_offer_bpp.discount_translation ();

ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD COLUMN discount_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.discount_translation ADD PRIMARY KEY ( discount_id, language);
