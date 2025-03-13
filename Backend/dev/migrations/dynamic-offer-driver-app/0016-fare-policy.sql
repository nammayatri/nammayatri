CREATE TABLE atlas_driver_offer_bpp.fare_policy_27_07_bak as SELECT * FROM atlas_driver_offer_bpp.fare_policy;


CREATE TABLE atlas_driver_offer_bpp.fare_policy_new (
id character(36) NOT NULL,
organization_id character (36) NOT NULL,

vehicle_variant text NOT NULL,
base_distance_per_km_fare double precision NOT NULL,
base_distance double precision NOT NULL,
extra_km_fare double precision NOT NULL,
dead_km_fare double precision NOT NULL,
driver_extra_fee_list double precision[] NOT NULL,

night_shift_start time without time zone,
night_shift_end time without time zone,
night_shift_rate double precision,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_new OWNER TO atlas_driver_offer_bpp_user;

--
CREATE TEMPORARY TABLE tmp_variants (variant text NOT NULL);
INSERT INTO tmp_variants VALUES ('SUV'), ('SEDAN'), ('HATCHBACK'), ('AUTO_VARIANT'), ('BIKE'), ('DELIVERY_BIKE'), ('DELIVERY_TRUCK_MINI'), ('DELIVERY_TRUCK_SMALL'), ('DELIVERY_TRUCK_MEDIUM'), ('DELIVERY_TRUCK_LARGE'), ('DELIVERY_TRUCK_ULTRA_LARGE');

INSERT INTO atlas_driver_offer_bpp.fare_policy_new
SELECT md5(random()::text || clock_timestamp()::text)::uuid, fp.organization_id, v.variant, 10, 3000, 12, 120, '{10, 20, 30}',
       fp.night_shift_start, fp.night_shift_end, fp.night_shift_rate, current_timestamp, current_timestamp
FROM atlas_driver_offer_bpp.fare_policy fp, tmp_variants as v;

DROP TABLE atlas_driver_offer_bpp.fare_policy;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_new RENAME TO fare_policy;

--
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN fare_for_pickup;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN distance_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN base_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN extra_km_fare double precision;

