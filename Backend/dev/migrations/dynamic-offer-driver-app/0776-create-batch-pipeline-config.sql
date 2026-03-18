CREATE TABLE atlas_driver_offer_bpp.batch_pipeline_config ();

ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN client_batch_size integer NOT NULL default 10;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN client_batch_interval_sec integer NOT NULL default 3;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN client_min_displacement_meters double precision NOT NULL default 25.0;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN client_location_update_interval_sec integer NOT NULL default 1;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN lts_drainer_size integer NOT NULL default 50;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN lts_drainer_delay_sec integer NOT NULL default 2;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN lts_batch_size integer NOT NULL default 20;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN normal_ride_bulk_loc_update_batch_size integer NOT NULL default 5;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN max_snap_to_road_req_points integer NOT NULL default 100;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN telemetry_sample_rate double precision NOT NULL default 0.1;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN telemetry_enabled boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN auto_tuning_enabled boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.batch_pipeline_config ADD PRIMARY KEY (id);

CREATE INDEX idx_batch_pipeline_config_merchant_city ON atlas_driver_offer_bpp.batch_pipeline_config USING btree (merchant_id, merchant_operating_city_id);

-- Audit table for tracking config changes made by the auto-tuner
CREATE TABLE atlas_driver_offer_bpp.batch_config_audit ();

ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN batch_pipeline_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN field_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN old_value text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN new_value text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN reason text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN changed_by text NOT NULL default 'auto_tuner';
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.batch_config_audit ADD PRIMARY KEY (id);

CREATE INDEX idx_batch_config_audit_config_id ON atlas_driver_offer_bpp.batch_config_audit USING btree (batch_pipeline_config_id);
CREATE INDEX idx_batch_config_audit_merchant_city ON atlas_driver_offer_bpp.batch_config_audit USING btree (merchant_id, merchant_operating_city_id);
