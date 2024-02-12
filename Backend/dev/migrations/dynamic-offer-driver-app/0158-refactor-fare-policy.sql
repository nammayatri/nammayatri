ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN driver_min_extra_fee DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN driver_max_extra_fee DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant ALTER COLUMN fare_policy_type DROP NOT NULL;

CREATE TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details (
  fare_parameters_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_driver_offer_bpp.fare_parameters(id),
  dead_km_fare integer NOT NULL,
  extra_km_fare integer
);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters_progressive_details OWNER TO atlas_driver_offer_bpp_user;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN fare_parameters_type character varying(50);
UPDATE atlas_driver_offer_bpp.fare_parameters AS T1 SET fare_parameters_type = 'Slab'
    WHERE T1.service_charge IS NOT NULL;
UPDATE atlas_driver_offer_bpp.fare_parameters AS T1 SET fare_parameters_type = 'Progressive'
    WHERE T1.fare_parameters_type IS NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN fare_parameters_type SET NOT NULL;

WITH ProgressiveFareParameters AS (
  SELECT T1.id,
    T1.dead_km_fare,
    T1.extra_km_fare
  FROM atlas_driver_offer_bpp.fare_parameters AS T1
)
INSERT INTO atlas_driver_offer_bpp.fare_parameters_progressive_details (
  fare_parameters_id,
  dead_km_fare,
  extra_km_fare)
  (SELECT * FROM ProgressiveFareParameters);

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN govt_charges integer;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN waiting_charge integer;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN night_shift_charge integer;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN dead_km_fare DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN extra_km_fare DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN night_shift_rate DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN night_coef_included DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN waiting_charge_per_min DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN waiting_or_pickup_charges DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN fare_policy_type DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ALTER COLUMN govt_charges_perc DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_policy
  ADD CONSTRAINT unique_fare_policy_id UNIQUE (id);

CREATE TABLE atlas_driver_offer_bpp.fare_policy_progressive_details (
  fare_policy_id character(36) PRIMARY KEY NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  base_distance integer NOT NULL,
  base_fare integer NOT NULL,
  per_extra_km_fare numeric (30,2) NOT NULL,
  dead_km_fare integer NOT NULL,
  waiting_charge JSON,
  night_shift_charge JSON
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab (
  id serial PRIMARY KEY,
  fare_policy_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.fare_policy(id),
  start_distance integer NOT NULL,
  base_fare integer NOT NULL,
  waiting_charge JSON,
  night_shift_charge JSON
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab OWNER TO atlas_driver_offer_bpp_user;

WITH ProgressiveFarePolicy AS (
  SELECT T1.id,
    T1.base_distance_meters,
    T1.base_distance_fare,
    T1.per_extra_km_fare,
    T1.dead_km_fare,
    CASE
        WHEN T1.waiting_charge_per_min IS NOT NULL
        THEN REPLACE ('{"contents":???,"tag":"PerMinuteWaitingCharge"}' :: text, '???' :: text, T1.waiting_charge_per_min :: text) :: json
	    ELSE NULL
	END,
    CASE
        WHEN T1.night_shift_rate IS NOT NULL
        THEN REPLACE ('{"contents":???,"tag":"ProgressiveNightShiftCharge"}' :: text, '???' :: text, T1.night_shift_rate :: text) :: json
	    ELSE NULL
	END
  FROM atlas_driver_offer_bpp.fare_policy AS T1
)
INSERT INTO atlas_driver_offer_bpp.fare_policy_progressive_details (
  fare_policy_id,
  base_distance,
  base_fare,
  per_extra_km_fare,
  dead_km_fare,
  waiting_charge,
  night_shift_charge)
  (SELECT * FROM ProgressiveFarePolicy);

ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN driver_min_extra_fee DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN driver_max_extra_fee DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN service_charge integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN govt_charges float;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN fare_policy_type character varying(50);
UPDATE atlas_driver_offer_bpp.fare_policy AS T1 SET fare_policy_type = 'Progressive';
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN fare_policy_type SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN base_distance_meters SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN base_distance_fare SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN per_extra_km_fare SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN dead_km_fare SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN waiting_charge_per_min SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN night_shift_rate SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ALTER COLUMN waiting_time_estimated_threshold SET NOT NULL;

-- INSERT INTO atlas_driver_offer_bpp.fare_policy (
--   T1.id,
--   T1.merchant_id,
--   T1.vehicle_variant,
--   T1.fare_policy_type,
--   T1.night_shift_start,
--   T1.night_shift_end,
--   T1.max_allowed_trip_distance,
--   T1.min_allowed_trip_distance,
--   T1.service_charge,
--   T1.govt_charges)
--   (SELECT
--     (T1.id,
--      T1.merchant_id,
--      T1.vehicle_variant,
--      'Slabs',
--      T1.night_shift_start,
--      T1.night_shift_end,
--      T1.max_allowed_trip_distance,
--      T1.min_allowed_trip_distance,
--      T1.service_charge,
--      T1.govt_charges)
--     FROM atlas_driver_offer_bpp.slab_fare_policy AS T1 );
-- Slabs are saved using show/read. It's rather difficult to migrate them

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.merchant DROP COLUMN fare_policy_type;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN dead_km_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN extra_km_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN night_shift_rate;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN night_coef_included;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN waiting_charge_per_min;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN waiting_or_pickup_charges;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN fare_policy_type;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters DROP COLUMN govt_charges_perc;

ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN base_distance_meters;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN base_distance_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN per_extra_km_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN dead_km_fare;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN waiting_charge_per_min;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN night_shift_rate;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP COLUMN waiting_time_estimated_threshold;

DROP TABLE atlas_driver_offer_bpp.slab_fare_policy;