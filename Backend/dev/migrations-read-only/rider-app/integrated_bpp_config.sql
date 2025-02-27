CREATE TABLE atlas_app.integrated_bpp_config ();

ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN platform_type text NOT NULL default 'APPLICATION';
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN config_json json NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN vehicle_category text NOT NULL default 'BUS';
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.integrated_bpp_config ADD PRIMARY KEY ( domain, id, merchant_id, merchant_operating_city_id, vehicle_category);
