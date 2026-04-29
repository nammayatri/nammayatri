CREATE TABLE atlas_driver_offer_bpp.restricted_extra_fare(
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character (36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    vehicle_variant text NOT NULL,
    min_trip_distance double precision NOT NULL,
    driver_max_extra_fare double precision NOT NULL
);