CREATE TABLE atlas_app.vehicle_trip ();

ALTER TABLE atlas_app.vehicle_trip ADD COLUMN capacity integer NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN driver_id text NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN end_reason text ;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN ended_at timestamp with time zone ;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN moving_at timestamp with time zone ;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN offline_boardings integer NOT NULL default 0;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN reached_end_at timestamp with time zone ;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN service_tier_type text NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN started_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.vehicle_trip ADD COLUMN vehicle_number text NOT NULL;
ALTER TABLE atlas_app.vehicle_trip ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY vehicle_trip_idx_driver_id_started_at ON atlas_app.vehicle_trip USING btree (driver_id, started_at);


------- SQL updates -------

CREATE INDEX CONCURRENTLY vehicle_trip_idx_merchant_operating_city_id_status ON atlas_app.vehicle_trip USING btree (merchant_operating_city_id, status);