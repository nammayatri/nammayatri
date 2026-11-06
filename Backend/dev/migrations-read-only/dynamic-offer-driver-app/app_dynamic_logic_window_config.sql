CREATE TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ();

ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN version integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN window_mode text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN window_size integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.app_dynamic_logic_window_config ADD PRIMARY KEY ( domain, version);
