ALTER TABLE atlas_app.hot_spot_config
  ADD COLUMN should_save_search_hot_spot boolean DEFAULT False,
  ADD COLUMN hot_spot_radius float DEFAULT 150.0,
  ADD COLUMN precision_to_set_geohash integer DEFAULT 9,
  ADD COLUMN precision_to_get_geohash integer DEFAULT 7,
  ADD COLUMN precision_to_filter_geohash integer DEFAULT 8,
  ADD COLUMN max_geo_hash_to_filter integer DEFAULT 1,
  ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  ADD COLUMN hot_spot_expiry integer DEFAULT 1296000;

ALTER TABLE atlas_app.hot_spot_config
  DROP COLUMN hot_spot_geo_hash_precision,
  DROP COLUMN nearby_geohash_precision;

UPDATE atlas_app.hot_spot_config
  SET
    min_frequency_of_hot_spot = 2,
    weight_of_manual_saved = 1,
    weight_of_auto_pickup = 1,
    weight_of_auto_saved = 1,
    weight_of_trip_start = 1,
    weight_of_trip_end = 1,
    weight_of_special_location = 1;