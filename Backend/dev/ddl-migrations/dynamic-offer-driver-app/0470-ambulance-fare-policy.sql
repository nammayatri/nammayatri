-- fare_parameters_ambulance_details
CREATE TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details();

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN dist_based_fare numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN platform_fee numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN cgst numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN sgst numeric(30, 2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD COLUMN currency character varying(255) NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters_ambulance_details ADD PRIMARY KEY (fare_parameters_id);

-- fare_policy_ambulance_details_slab
CREATE TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab();

ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN id serial PRIMARY KEY;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN fare_policy_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN vehicle_age int NOT NULL; -- months(should we still take numeric?)
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN base_fare numeric(30, 2) NOT NULL; -- value?
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN per_km_rate numeric(30, 2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN night_shift_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN waiting_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN free_waiting_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN platform_fee_charge json;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN platform_fee_cgst double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_ambulance_details_slab ADD COLUMN platform_fee_sgst double precision;

alter table atlas_driver_offer_bpp.quote_special_zone add column min_estimated_fare double precision;
alter table atlas_driver_offer_bpp.quote_special_zone add column max_estimated_fare double precision;
