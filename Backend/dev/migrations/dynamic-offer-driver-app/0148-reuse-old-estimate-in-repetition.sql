CREATE TABLE atlas_driver_offer_bpp.estimate (
  id character(36) PRIMARY KEY NOT NULL,
  transaction_id character(36) NOT NULL,
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

WITH Estimates AS (
  SELECT T1.id,
    T1.transaction_id,
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
  FROM atlas_driver_offer_bpp.search_request AS T1
  LEFT JOIN atlas_driver_offer_bpp.search_request_for_driver AS T2
    ON T1.id = T2.search_request_id
)
INSERT INTO atlas_driver_offer_bpp.estimate (
  id,
  transaction_id,
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

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN transaction_id character(36);
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET transaction_id = (SELECT T2.transaction_id FROM atlas_driver_offer_bpp.search_request AS T2 WHERE T2.id = T1.search_request_id);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN transaction_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN transaction_id character(36);
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET transaction_id = (SELECT T2.transaction_id FROM atlas_driver_offer_bpp.search_request AS T2 WHERE T2.id = T1.search_request_id);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN transaction_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN estimate_id character(36);
UPDATE atlas_driver_offer_bpp.search_request SET estimate_id = id;
ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN estimate_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD CONSTRAINT
  search_request_to_estimate_fk FOREIGN KEY (estimate_id) REFERENCES atlas_driver_offer_bpp.estimate (id);