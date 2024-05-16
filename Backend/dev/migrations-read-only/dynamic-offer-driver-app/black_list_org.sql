CREATE TABLE atlas_driver_offer_bpp.black_list_org ();

ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN domain character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD COLUMN subscriber_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.black_list_org ADD PRIMARY KEY ( id);