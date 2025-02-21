CREATE TABLE atlas_app.frfs_route_fare_product ();

ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN vehicle_service_tier_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN vehicle_type text NOT NULL;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_route_fare_product ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_route_fare_product ADD PRIMARY KEY ( id);
