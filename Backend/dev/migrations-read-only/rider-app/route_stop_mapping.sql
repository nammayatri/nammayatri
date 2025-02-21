CREATE TABLE atlas_app.route_stop_mapping ();

ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN estimated_travel_time_from_previous_stop integer ;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN provider_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN sequence_num integer NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN stop_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN stop_name text NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN stop_lat double precision NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN stop_lon double precision NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_mapping ADD PRIMARY KEY ( route_code, stop_code);
