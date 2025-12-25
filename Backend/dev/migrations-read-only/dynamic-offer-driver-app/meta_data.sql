CREATE TABLE atlas_driver_offer_bpp.meta_data ();

ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN app_permissions text ;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN device text ;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN device_date_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN device_os text ;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.meta_data ADD PRIMARY KEY ( driver_id);