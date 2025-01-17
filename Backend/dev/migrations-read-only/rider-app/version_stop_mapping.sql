CREATE TABLE atlas_app.version_stop_mapping ();

ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN failure_reason text ;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN stage_data text ;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN stage_id text NOT NULL;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN version_id text NOT NULL;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.version_stop_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.version_stop_mapping ADD PRIMARY KEY ( id);
