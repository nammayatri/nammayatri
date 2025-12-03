CREATE TABLE atlas_app.frfs_vehicle_service_tier ();

ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN integrated_bpp_config_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN long_name text NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN provider_code text NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN short_name text NOT NULL;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN is_air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_vehicle_service_tier ADD COLUMN train_type text ;