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
    go_home_to_location_radius integer NOT NULL DEFAULT 2000,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.go_home_config (id, enable_go_home, start_cnt, dest_radius, active_time, update_home_location_after_sec, cancecllation_cnt, created_at, updated_at) VALUES ('favorit0-0000-0000-0000-00000favorit', true, 2, 3000, 1800, 2592000, 2, 5, now(), now());
