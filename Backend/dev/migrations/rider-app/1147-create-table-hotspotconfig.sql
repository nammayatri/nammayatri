WITH HotSpotConfigs AS (
    SELECT T1.id,
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
           True
    FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.hot_spot_config
    (
    id,
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
    should_take_hot_spot
    )
SELECT *
FROM HotSpotConfigs;