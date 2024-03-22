UPDATE atlas_app.hot_spot_config
  SET
    min_frequency_of_hot_spot = 2,
    weight_of_manual_saved = 1,
    weight_of_auto_pickup = 1,
    weight_of_auto_saved = 1,
    weight_of_trip_start = 1,
    weight_of_trip_end = 1,
    weight_of_special_location = 1;