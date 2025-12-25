CREATE TABLE atlas_app.route_stop_fare ();

ALTER TABLE atlas_app.route_stop_fare ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN currency text NOT NULL default 'INR';
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN end_stop_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN start_stop_code text NOT NULL;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_fare ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.route_stop_fare ADD PRIMARY KEY ( end_stop_code, fare_policy_id, start_stop_code);