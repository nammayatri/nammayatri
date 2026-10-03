-- NOTE: dont need to run these queries
CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs (
    id serial PRIMARY KEY,
    fare_policy_id character(36) NOT NULL,
    time_percentage int NOT NULL,
    distance_percentage int NOT NULL,
    fare_percentage int NOT NULL,
    include_actual_time_percentage boolean NOT NULL,
    include_actual_dist_percentage boolean NOT NULL
);

CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs (
    id serial PRIMARY KEY,
    fare_policy_id character(36) NOT NULL,
    time_percentage int NOT NULL,
    distance_percentage int NOT NULL,
    fare_percentage int NOT NULL,
    include_actual_time_percentage boolean NOT NULL,
    include_actual_dist_percentage boolean NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN IF NOT EXISTS buffer_meters integer;-- NOTE: dont need to run these queries
-- NOTE: dont need to run these queries
