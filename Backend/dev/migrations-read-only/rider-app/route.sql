CREATE TABLE atlas_app.route ();

ALTER TABLE atlas_app.route ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN color text ;
ALTER TABLE atlas_app.route ADD COLUMN end_lat double precision NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN end_lon double precision NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN long_name text NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN polyline text ;
ALTER TABLE atlas_app.route ADD COLUMN short_name text NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN start_lat double precision NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN start_lon double precision NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_app.route ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.route ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.route ADD COLUMN stop_count integer ;
ALTER TABLE atlas_app.route ADD COLUMN daily_trip_count integer ;