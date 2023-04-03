CREATE TABLE atlas_driver_offer_bpp.search_request (
  id character(36) PRIMARY KEY NOT NULL,
  transaction_id character(36) NOT NULL,
  provider_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
  from_location_id character varying(36) REFERENCES atlas_driver_offer_bpp.search_request_location (id),
  to_location_id character varying(36) REFERENCES atlas_driver_offer_bpp.search_request_location (id),
  bap_id character varying(255) NOT NULL,
  bap_uri character varying(255) NOT NULL,
  estimated_distance int NOT NULL,
  estimated_duration int NOT NULL,
  auto_assign_enabled boolean NOT NULL,
  customer_language character varying(36),
  created_at timestamp with time zone  NOT NULL DEFAULT now()
);
ALTER TABLE atlas_driver_offer_bpp.search_request OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.estimate (
  id character(36) PRIMARY KEY NOT NULL,
  request_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.search_request (id),
  vehicle_variant character varying(36) NOT NULL,
  min_fare int NOT NULL,
  max_fare int NOT NULL,
  estimate_breakup_list text[] NOT NULL,
  night_shift_multiplier numeric(30,2),
  night_shift_start character varying(255),
  night_shift_end character varying(255),
  waiting_time_estimated_threshold int,
  waiting_charge_per_min int,
  waiting_or_pickup_charges int,
  created_at timestamp with time zone NOT NULL DEFAULT now()
);
ALTER TABLE atlas_driver_offer_bpp.estimate OWNER TO atlas_driver_offer_bpp_user;

WITH SearchRequests AS (
  SELECT T1.id,
    T1.transaction_id,
    T1.provider_id,
    T1.from_location_id,
    T1.to_location_id,
    T1.bap_id,
    T1.bap_uri,
    T1.estimated_distance,
    T1.estimated_duration,
    T1.auto_assign_enabled,
    NULL,
    T1.created_at
  FROM atlas_driver_offer_bpp.search_step AS T1
)
INSERT INTO atlas_driver_offer_bpp.search_request (
  id,
  transaction_id,
  provider_id,
  from_location_id,
  to_location_id,
  bap_id,
  bap_uri,
  estimated_distance,
  estimated_duration,
  auto_assign_enabled,
  customer_language,
  created_at)
  (SELECT * FROM SearchRequests);

WITH Estimates AS (
  SELECT T1.id,
    T1.id,
    T1.vehicle_variant,
    T2.base_fare + T2.driver_min_extra_fee, -- == min_fare
    T2.base_fare + T2.driver_max_extra_fee, -- == max_fare
    ARRAY ['UNKNOWN'],
    -- NULL, -- == night_shift_multiplier
    -- NULL, -- == night_shift_start
    -- NULL, -- == night_shift_end
    0, -- == waiting_time_estimated_threshold
    0, -- == waiting_charge_per_min
    0, -- == waiting_or_pickup_charges
    T1.created_at
  FROM atlas_driver_offer_bpp.search_step AS T1
  LEFT JOIN atlas_driver_offer_bpp.search_request_for_driver AS T2
    ON T1.id = T2.search_request_id
)
INSERT INTO atlas_driver_offer_bpp.estimate (
  id,
  request_id,
  vehicle_variant,
  min_fare,
  max_fare,
  estimate_breakup_list,
  -- night_shift_multiplier,
  -- night_shift_start,
  -- night_shift_end,
  waiting_time_estimated_threshold,
  waiting_charge_per_min,
  waiting_or_pickup_charges,
  created_at)
  (SELECT * FROM Estimates);

ALTER TABLE atlas_driver_offer_bpp.driver_quote RENAME COLUMN search_request_id to search_step_id;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN request_id character(36);
UPDATE atlas_driver_offer_bpp.driver_quote SET request_id = search_step_id;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN request_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD CONSTRAINT
  driver_quote_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request (id);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver RENAME COLUMN search_request_id to search_step_id;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN request_id character(36);
UPDATE atlas_driver_offer_bpp.search_request_for_driver SET request_id = search_step_id;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN request_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD CONSTRAINT
  search_request_for_driver_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request (id);

ALTER TABLE atlas_driver_offer_bpp.search_step ADD COLUMN request_id character(36);
UPDATE atlas_driver_offer_bpp.search_step SET request_id = id;
ALTER TABLE atlas_driver_offer_bpp.search_step ALTER COLUMN request_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_step ADD CONSTRAINT
  search_step_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request (id);

ALTER TABLE atlas_driver_offer_bpp.search_step ADD COLUMN estimate_id character(36);
UPDATE atlas_driver_offer_bpp.search_step SET estimate_id = id;
ALTER TABLE atlas_driver_offer_bpp.search_step ALTER COLUMN estimate_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_step ADD CONSTRAINT
  search_step_to_estimate_fk FOREIGN KEY (estimate_id) REFERENCES atlas_driver_offer_bpp.estimate (id);

ALTER TABLE atlas_driver_offer_bpp.search_step
    DROP COLUMN provider_id,
    DROP COLUMN from_location_id,
    DROP COLUMN to_location_id,
    DROP COLUMN bap_id,
    DROP COLUMN bap_uri,
    DROP COLUMN estimated_distance,
    DROP COLUMN estimated_duration,
    DROP COLUMN transaction_id,
    DROP COLUMN auto_assign_enabled;
