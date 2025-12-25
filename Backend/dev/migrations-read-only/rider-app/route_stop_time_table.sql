CREATE TABLE atlas_app.route_stop_time_table ();

ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN service_tier_type text NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN stop_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN time_of_arrival time without time zone NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN time_of_departure time without time zone NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN trip_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_time_table ADD PRIMARY KEY ( integrated_bpp_config_id, trip_id);



------- SQL updates -------

ALTER TABLE atlas_app.route_stop_time_table DROP CONSTRAINT route_stop_time_table_pkey;
ALTER TABLE atlas_app.route_stop_time_table ADD PRIMARY KEY ( integrated_bpp_config_id, stop_code, time_of_arrival, trip_id);


------- SQL updates -------

ALTER TABLE atlas_app.route_stop_time_table DROP CONSTRAINT route_stop_time_table_pkey;
ALTER TABLE atlas_app.route_stop_time_table ADD PRIMARY KEY ( integrated_bpp_config_id, service_tier_type, stop_code, time_of_arrival, trip_id);


------- SQL updates -------

ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN delay integer ;


------- SQL updates -------

ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN source text ;


------- SQL updates -------

ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN stage integer ;


------- SQL updates -------

ALTER TABLE atlas_app.route_stop_time_table ADD COLUMN platform_code text ;