CREATE TABLE atlas_app.route_trip_mapping ();

ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN trip_code text NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN trip_end_time text NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN trip_start_time text NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_trip_mapping ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.route_trip_mapping ADD PRIMARY KEY ( trip_code);
