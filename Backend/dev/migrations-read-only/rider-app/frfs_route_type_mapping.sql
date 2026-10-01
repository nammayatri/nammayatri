CREATE TABLE atlas_app.frfs_route_type_mapping ();

ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN route_type text NOT NULL;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN vehicle_service_tier_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_route_type_mapping ADD PRIMARY KEY ( integrated_bpp_config_id, route_code, vehicle_service_tier_id);
