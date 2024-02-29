CREATE TABLE atlas_driver_offer_bpp.bap_metadata ();

ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN logo_url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.bap_metadata ADD PRIMARY KEY ( id);