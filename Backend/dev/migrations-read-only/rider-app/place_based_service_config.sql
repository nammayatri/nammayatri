CREATE TABLE atlas_app.place_based_service_config ();

ALTER TABLE atlas_app.place_based_service_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.place_based_service_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.place_based_service_config ADD COLUMN place_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.place_based_service_config ADD COLUMN config_value json NOT NULL;
ALTER TABLE atlas_app.place_based_service_config ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_app.place_based_service_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.place_based_service_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.place_based_service_config ADD PRIMARY KEY ( place_id);