CREATE TABLE atlas_app.hot_spot_config ();

ALTER TABLE atlas_app.hot_spot_config ADD COLUMN block_radius integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN hot_spot_expiry integer NOT NULL default 1296000;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN hot_spot_radius double precision NOT NULL default 150.0;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN max_geo_hash_to_filter integer NOT NULL default 1;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN max_num_hot_spots_to_show integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN min_frequency_of_hot_spot integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN precision_to_filter_geohash integer NOT NULL default 8;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN precision_to_get_geohash integer NOT NULL default 7;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN precision_to_set_geohash integer NOT NULL default 9;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN should_save_search_hot_spot boolean NOT NULL default False;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN should_take_hot_spot boolean NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_auto_pickup integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_auto_saved integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_manual_pickup integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_manual_saved integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_special_location integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_trip_end integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD COLUMN weight_of_trip_start integer NOT NULL;
ALTER TABLE atlas_app.hot_spot_config ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.hot_spot_config DROP COLUMN updated_at;
ALTER TABLE atlas_app.hot_spot_config DROP COLUMN created_at;