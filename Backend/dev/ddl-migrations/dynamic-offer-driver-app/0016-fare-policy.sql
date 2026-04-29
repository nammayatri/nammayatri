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

DROP TABLE atlas_driver_offer_bpp.fare_policy;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_new RENAME TO fare_policy;

--
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN fare_for_pickup;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN distance_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN base_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN extra_km_fare double precision;

