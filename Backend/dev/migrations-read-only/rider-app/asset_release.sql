CREATE TABLE atlas_app.asset_release ();

ALTER TABLE atlas_app.asset_release ADD COLUMN asset_type text NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.asset_release ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN merchant_id character(36) NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN merchant_operating_city_id character(36) NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN rolled_back_at timestamp with time zone ;
ALTER TABLE atlas_app.asset_release ADD COLUMN sha256 text NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN size_bytes integer NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN source_ref text ;
ALTER TABLE atlas_app.asset_release ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.asset_release ADD COLUMN url text NOT NULL;
ALTER TABLE atlas_app.asset_release ADD COLUMN version text NOT NULL;
ALTER TABLE atlas_app.asset_release ADD PRIMARY KEY ( id);
