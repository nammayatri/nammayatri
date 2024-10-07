CREATE TABLE atlas_app.integrated_bpp_config ();

ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN domain text NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN qr_generated_by text NOT NULL default 'BAP';
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN qr_generation_key text ;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN qr_verification_key text ;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN qr_verified_by text NOT NULL default 'BPP';
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN vehicle_category text NOT NULL default 'BUS';
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.integrated_bpp_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.integrated_bpp_config ADD PRIMARY KEY ( id);