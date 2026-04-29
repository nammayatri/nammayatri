CREATE TABLE atlas_driver_offer_bpp.fare_policy_inter_city_details_pricing_slabs (
    id serial PRIMARY KEY,
    fare_policy_id character(36) NOT NULL,
    time_percentage int NOT NULL,
    distance_percentage int NOT NULL,
    fare_percentage int NOT NULL,
    include_actual_time_percentage boolean NOT NULL,
    include_actual_dist_percentage boolean NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.fare_policy_rental_details_pricing_slabs (
    id serial PRIMARY KEY,
    fare_policy_id character(36) NOT NULL,
    time_percentage int NOT NULL,
    distance_percentage int NOT NULL,
    fare_percentage int NOT NULL,
    include_actual_time_percentage boolean NOT NULL,
    include_actual_dist_percentage boolean NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.fare_policy_rental_details_distance_buffers ADD COLUMN buffer_meters integer;