CREATE TABLE atlas_app.frfs_fare_class ();

ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN class_code text NOT NULL;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN display_name text NOT NULL;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN route_tag text ;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN time_bounds text NOT NULL default 'Unbounded';
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN vehicle_service_tier_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN vehicle_tag text ;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN vehicle_variant text ;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_fare_class ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_fare_class ADD PRIMARY KEY ( id);
