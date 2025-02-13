CREATE TABLE atlas_app.version ();

ALTER TABLE atlas_app.version ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.version ADD COLUMN input_data_type text NOT NULL;
ALTER TABLE atlas_app.version ADD COLUMN is_ready_to_apply boolean NOT NULL;
ALTER TABLE atlas_app.version ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.version ADD COLUMN version_tag integer NOT NULL;
ALTER TABLE atlas_app.version ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.version ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.version ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.version ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.version ADD PRIMARY KEY ( id);
ALTER TABLE atlas_app.version ADD COLUMN gtfs_link text ;