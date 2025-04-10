CREATE TABLE atlas_app.route_stop_calender ();

ALTER TABLE atlas_app.route_stop_calender ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_calender ADD COLUMN serviceability integer[] NOT NULL;
ALTER TABLE atlas_app.route_stop_calender ADD COLUMN trip_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_calender ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.route_stop_calender ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.route_stop_calender ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_calender ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_calender ADD PRIMARY KEY ( integrated_bpp_config_id, trip_id);
