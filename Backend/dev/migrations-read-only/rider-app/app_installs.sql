CREATE TABLE atlas_app.app_installs ();

ALTER TABLE atlas_app.app_installs ADD COLUMN app_version text ;
ALTER TABLE atlas_app.app_installs ADD COLUMN bundle_version text ;
ALTER TABLE atlas_app.app_installs ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.app_installs ADD COLUMN device_token text NOT NULL;
ALTER TABLE atlas_app.app_installs ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.app_installs ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.app_installs ADD COLUMN platform text ;
ALTER TABLE atlas_app.app_installs ADD COLUMN source text NOT NULL;
ALTER TABLE atlas_app.app_installs ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.app_installs ADD PRIMARY KEY ( id);