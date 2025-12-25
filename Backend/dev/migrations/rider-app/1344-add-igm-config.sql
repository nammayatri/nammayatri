CREATE TABLE atlas_app.igm_config ();

ALTER TABLE atlas_app.igm_config ADD COLUMN gro_email text NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN gro_name text NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN gro_phone text NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN expected_response_time integer NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN expected_resolution_time integer NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.igm_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.igm_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.igm_config ADD PRIMARY KEY ( id);