CREATE TABLE atlas_driver_offer_bpp.estimate_revised (
    id character(36) NOT NULL PRIMARY KEY,
    request_id character (36) NOT NULL REFERENCES atlas_driver_offer_bpp.search_request (id),
    vehicle_variant character varying(60) NOT NULL,
    min_fare int NOT NULL,
    max_fare int NOT NULL,
    special_location_tag text,
    trip_category text,
    estimated_distance integer,
    fare_params_id character varying(36),
    fare_policy_id character varying(36),
    updated_at timestamp with time zone,
    is_scheduled boolean,
    created_at timestamp with time zone NOT NULL DEFAULT now()
);
ALTER TABLE atlas_driver_offer_bpp.estimate_revised OWNER TO atlas_driver_offer_bpp_user;

WITH Estimates AS (
  SELECT T1.id,
    T2.vehicle_variant,
    T3.base_fare + T2.driver_min_extra_fee, -- == min_fare
    T3.base_fare + T2.driver_max_extra_fee, -- == max_fare
    T1.created_at
  FROM atlas_driver_offer_bpp.search_request AS T1
  LEFT JOIN atlas_driver_offer_bpp.search_request_for_driver AS T2
    ON T1.id = T2.search_request_id
  JOIN atlas_driver_offer_bpp.search_try AS T3
    ON T3.id = T2.search_try_id
)
INSERT INTO atlas_driver_offer_bpp.estimate_revised (
  id,
  vehicle_variant,
  min_fare,
  max_fare,
  created_at)
  (SELECT * FROM Estimates);

