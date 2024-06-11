CREATE TABLE atlas_driver_offer_bpp.restricted_extra_fare(
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character (36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    vehicle_variant text NOT NULL,
    min_trip_distance double precision NOT NULL,
    driver_max_extra_fare double precision NOT NULL
);


INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('7d092382-7634-eb1a-b9b9-27a2ff657f48','favorit0-0000-0000-0000-00000favorit','SUV',0,10);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('385b5b27-63b0-a63e-a0a5-947484416a58','favorit0-0000-0000-0000-00000favorit','SEDAN',0,10);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('7d092382-7634-eb1a-b9b9-27a2ff657f49','favorit0-0000-0000-0000-00000favorit','HATCHBACK',0,10);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('6d092382-7634-eb1a-b9b9-27a2ff657f48','favorit0-0000-0000-0000-00000favorit','AUTO_RICKSHAW',0,10);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('485b5b27-63b0-a63e-a0a5-947484416a58','favorit0-0000-0000-0000-00000favorit','BIKE',0,10);


INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('1d092382-7634-eb1a-b9b9-27a2ff657f48','favorit0-0000-0000-0000-00000favorit','SUV',3,20);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('185b5b27-63b0-a63e-a0a5-947484416a58','favorit0-0000-0000-0000-00000favorit','SEDAN',3,20);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('9d092382-7634-eb1a-b9b9-27a2ff657f49','favorit0-0000-0000-0000-00000favorit','HATCHBACK',3,20);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('5d092382-7634-eb1a-b9b9-27a2ff657f48','favorit0-0000-0000-0000-00000favorit','AUTO_RICKSHAW',3,20);

INSERT INTO atlas_driver_offer_bpp.restricted_extra_fare VALUES ('285b5b27-63b0-a63e-a0a5-947484416a58','favorit0-0000-0000-0000-00000favorit','BIKE',3,20);