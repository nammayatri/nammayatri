CREATE TABLE atlas_driver_offer_bpp.go_home_config (
    merchant_id character(36) NOT NULL PRIMARY KEY,
    enable_go_home boolean NOT NULL DEFAULT true,
    start_cnt integer NOT NULL DEFAULT 2,
    dest_radius integer NOT NULL DEFAULT 3000,
    active_time integer NOT NULL DEFAULT 1800,
    update_home_location_after_sec integer NOT NULL DEFAULT 2592000,
    cancecllation_cnt integer NOT NULL DEFAULT 2,
    num_home_locations integer NOT NULL DEFAULT 5,
    go_home_from_location_radius integer NOT NULL DEFAULT 7000,
    go_home_way_point_radius integer NOT NULL DEFAULT 2000,
    num_drivers_for_dir_check integer NOT NULL DEFAULT 5,
    go_home_batch_delay integer NOT NULL DEFAULT 4,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.go_home_config (merchant_id, enable_go_home, start_cnt, dest_radius, active_time, update_home_location_after_sec, cancecllation_cnt, num_home_locations, go_home_from_location_radius, go_home_way_point_radius, num_drivers_for_dir_check, go_home_batch_delay, created_at, updated_at)
SELECT
    id,
    true,
    2,
    3000,
    1800,
    2592000,
    2,
    5,
    7000,
    2000,
    5,
    4,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_driver_offer_bpp.merchant;