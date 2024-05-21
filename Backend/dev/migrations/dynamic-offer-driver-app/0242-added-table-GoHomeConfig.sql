INSERT INTO atlas_driver_offer_bpp.go_home_config (merchant_id, enable_go_home, start_cnt, cancellation_cnt, active_time, update_home_location_after_sec, num_home_locations, go_home_from_location_radius, go_home_way_point_radius, num_drivers_for_dir_check, go_home_batch_delay, add_start_waypoint_at, dest_radius_meters, ignore_waypoints_till, merchant_operating_city_id, new_loc_allowed_radius, created_at, updated_at)
SELECT
    id,
    true,
    2,
    2,
    3000,
    2592000,
    2,
    7000,
    2000,
    5,
    4,
    3000,
    3000,
    3000,
    atlas_driver_offer_bpp.uuid_generate_v4(),
    20,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant;
