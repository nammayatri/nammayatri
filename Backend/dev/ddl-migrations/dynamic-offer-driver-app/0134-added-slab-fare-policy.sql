CREATE TABLE atlas_driver_offer_bpp.slab_fare_policy (
id character(36) NOT NULL,
merchant_id character (36) NOT NULL,

vehicle_variant text NOT NULL,

night_shift_start time without time zone,
night_shift_end time without time zone,
night_shift_rate double precision,

min_allowed_trip_distance integer,
max_allowed_trip_distance integer,

service_charge integer NOT NULL,
fare_slabs text[] NOT NULL,
govt_charges_perc int,

created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  slab_fare_policy_merchant_id_fkey FOREIGN KEY (merchant_id) REFERENCES atlas_driver_offer_bpp.merchant(id)
);
alter table atlas_driver_offer_bpp.fare_parameters add column govt_charges_perc int;
