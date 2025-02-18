CREATE TABLE atlas_app.bbps_config ();

ALTER TABLE atlas_app.bbps_config ADD COLUMN bbps_agent_id text NOT NULL;
ALTER TABLE atlas_app.bbps_config ADD COLUMN bbps_server_url text NOT NULL;
ALTER TABLE atlas_app.bbps_config ADD COLUMN bbps_signature_key text NOT NULL;
ALTER TABLE atlas_app.bbps_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.bbps_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.bbps_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.bbps_config ADD PRIMARY KEY ( merchant_id);
