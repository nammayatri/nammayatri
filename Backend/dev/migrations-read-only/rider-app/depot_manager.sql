CREATE TABLE atlas_app.depot_manager ();

ALTER TABLE atlas_app.depot_manager ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.depot_manager ADD COLUMN depot_code character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot_manager ADD COLUMN enabled boolean NOT NULL;
ALTER TABLE atlas_app.depot_manager ADD COLUMN is_admin boolean NOT NULL;
ALTER TABLE atlas_app.depot_manager ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot_manager ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot_manager ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.depot_manager ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.depot_manager ADD PRIMARY KEY ( depot_code, person_id);
