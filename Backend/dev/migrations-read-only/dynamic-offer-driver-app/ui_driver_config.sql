CREATE TABLE atlas_driver_offer_bpp.ui_driver_config ();

ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN config json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN os text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN platform text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.ui_driver_config ADD PRIMARY KEY ( id);
