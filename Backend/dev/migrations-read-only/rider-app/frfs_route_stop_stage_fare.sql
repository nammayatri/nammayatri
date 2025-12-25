CREATE TABLE atlas_app.frfs_route_stop_stage_fare ();

ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN stage integer NOT NULL;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN stop_code text NOT NULL;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_route_stop_stage_fare ADD PRIMARY KEY ( fare_policy_id, route_code, stop_code);