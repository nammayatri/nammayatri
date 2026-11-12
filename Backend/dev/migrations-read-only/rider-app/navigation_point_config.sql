CREATE TABLE atlas_app.navigation_point_config ();

ALTER TABLE atlas_app.navigation_point_config ADD COLUMN bucket_geohash_precision integer NOT NULL default 7;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN enabled boolean NOT NULL default false;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN max_points_to_show integer NOT NULL default 3;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN point_geohash_precision integer NOT NULL default 9;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN radius_in_meters double precision NOT NULL default 150.0;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN required_field_mask text ;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN ttl_in_seconds integer NOT NULL default 2592000;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.navigation_point_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.navigation_point_config ADD PRIMARY KEY ( merchant_operating_city_id);
