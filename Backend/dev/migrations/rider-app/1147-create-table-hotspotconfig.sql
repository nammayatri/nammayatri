CREATE TABLE atlas_app.hot_spot_config (
    id Text,
    hot_spot_geo_hash_precision Int,
    nearby_geohash_precision Int,
    block_radius Int,
    min_frequency_of_hot_spot Int,
    weight_of_manual_pickup Int,
    weight_of_manual_saved Int,
    weight_of_auto_pickup Int,
    weight_of_auto_saved Int,
    weight_of_trip_start Int,
    max_num_hot_spots_to_show Int,
    weight_of_trip_end Int,
    weight_of_special_location Int,
    should_take_hot_spot Boolean
);

WITH HotSpotConfigs AS (
    SELECT T1.id,
           8,
           10000,
           0,
           1,
           10000,
           10000,
           10000,
           10000,
           10000,
           10000,
           10000,
           True,
           7
    FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.hot_spot_config
    (
    id,
    hot_spot_geo_hash_precision,
    block_radius,
    min_frequency_of_hot_spot,
    weight_of_manual_pickup,
    weight_of_manual_saved,
    weight_of_auto_pickup,
    weight_of_auto_saved,
    weight_of_trip_start,
    max_num_hot_spots_to_show,
    weight_of_trip_end,
    weight_of_special_location,
    should_take_hot_spot,
    nearby_geohash_precision
    )
SELECT *
FROM HotSpotConfigs;