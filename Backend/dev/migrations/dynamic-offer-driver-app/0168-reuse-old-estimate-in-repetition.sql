ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN customer_language character(36);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN search_try_id character(36);
CREATE INDEX idx_driver_quote_s_try_id ON atlas_driver_offer_bpp.driver_quote USING btree (search_try_id);
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET search_try_id = search_request_id WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN search_try_id character(36);
CREATE INDEX idx_search_request_for_driver_s_try_id ON atlas_driver_offer_bpp.search_request_for_driver USING btree (search_try_id);
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET search_try_id = search_request_id WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN request_id character(36);
CREATE INDEX idx_search_try_s_req_id ON atlas_driver_offer_bpp.search_try USING btree (request_id);
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET request_id = id;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN transaction_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.estimate ALTER COLUMN transaction_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN request_id character(36);
UPDATE atlas_driver_offer_bpp.estimate AS T1 SET request_id = (SELECT T2.id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T2.estimate_id = T1.id) WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN transaction_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request
    ALTER COLUMN message_id DROP NOT NULL,
    ALTER COLUMN estimate_id DROP NOT NULL,
    ALTER COLUMN start_time DROP NOT NULL,
    ALTER COLUMN valid_till DROP NOT NULL,
    ALTER COLUMN customer_extra_fee DROP NOT NULL,
    ALTER COLUMN status DROP NOT NULL,
    ALTER COLUMN vehicle_variant DROP NOT NULL,
    ALTER COLUMN search_repeat_counter DROP NOT NULL,
    ALTER COLUMN updated_at DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_try
    ALTER COLUMN provider_id DROP NOT NULL,
    ALTER COLUMN from_location_id DROP NOT NULL,
    ALTER COLUMN to_location_id DROP NOT NULL,
    ALTER COLUMN bap_id DROP NOT NULL,
    ALTER COLUMN bap_uri DROP NOT NULL,
    ALTER COLUMN estimated_distance DROP NOT NULL,
    ALTER COLUMN estimated_duration DROP NOT NULL,
    ALTER COLUMN transaction_id DROP NOT NULL,
    ALTER COLUMN auto_assign_enabled DROP NOT NULL,
    ALTER COLUMN device DROP NOT NULL;

UPDATE atlas_driver_offer_bpp.driver_flow_status AS T1 SET
  flow_status = CAST (CAST (T1.flow_status AS jsonb) || jsonb_build_object ('searchTryId', T1.flow_status->>'requestId') AS json)
  WHERE (T1.flow_status->>'status') = 'GOT_SEARCH_REQUEST';

UPDATE atlas_driver_offer_bpp.scheduler_job AS T1 SET
  job_data = CAST (CAST (T1.job_data AS jsonb) || jsonb_build_object ('searchTryId', CAST (T1.job_data AS jsonb)->>'requestId') AS json)
  WHERE T1.job_type = 'SendSearchRequestToDriver';

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET search_try_id = search_request_id WHERE T1.search_try_id IS NULL;
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET search_try_id = search_request_id WHERE T1.search_try_id IS NULL;
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET request_id = id WHERE T1.request_id IS NULL;
UPDATE atlas_driver_offer_bpp.estimate AS T1 SET request_id = (SELECT T2.id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T2.estimate_id = T1.id limit 1) WHERE T1.request_id IS NULL;

INSERT INTO atlas_driver_offer_bpp.search_try (
  id,
  transaction_id,
  message_id,
  estimate_id,
  start_time,
  valid_till,
  provider_id,
  from_location_id,
  to_location_id,
  bap_id,
  bap_uri,
  estimated_distance,
  estimated_duration,
  customer_extra_fee,
  device,
  status,
  vehicle_variant,
  search_repeat_counter,
  auto_assign_enabled,
  created_at,
  updated_at
  )
  (SELECT id,
    transaction_id,
    message_id,
    estimate_id,
    start_time,
    valid_till,
    provider_id,
    from_location_id,
    to_location_id,
    bap_id,
    bap_uri,
    estimated_distance,
    estimated_duration,
    customer_extra_fee,
    device,
    status,
    vehicle_variant,
    search_repeat_counter,
    auto_assign_enabled,
    created_at,
    updated_at FROM atlas_driver_offer_bpp.search_request as T1 WHERE NOT EXISTS (SELECT id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T1.id = T2.id));

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD CONSTRAINT
  driver_quote_to_search_try_fk FOREIGN KEY (search_try_id) REFERENCES atlas_driver_offer_bpp.search_try (id);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD CONSTRAINT
  search_request_for_driver_to_search_try_fk FOREIGN KEY (search_try_id) REFERENCES atlas_driver_offer_bpp.search_try (id);

ALTER TABLE atlas_driver_offer_bpp.search_try ADD CONSTRAINT
  search_step_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request (id);

ALTER TABLE atlas_driver_offer_bpp.estimate ADD CONSTRAINT
  estimate_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request (id);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN search_try_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN search_try_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN request_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.estimate ALTER COLUMN request_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
    DROP COLUMN transaction_id;

ALTER TABLE atlas_driver_offer_bpp.estimate
    DROP COLUMN transaction_id;

ALTER TABLE atlas_driver_offer_bpp.driver_quote
    DROP COLUMN transaction_id;

ALTER TABLE atlas_driver_offer_bpp.search_request
    DROP COLUMN message_id,
    DROP COLUMN estimate_id,
    DROP COLUMN start_time,
    DROP COLUMN valid_till,
    DROP COLUMN customer_extra_fee,
    DROP COLUMN status,
    DROP COLUMN vehicle_variant,
    DROP COLUMN search_repeat_counter,
    DROP COLUMN updated_at;

ALTER TABLE atlas_driver_offer_bpp.search_try
    DROP COLUMN provider_id,
    DROP COLUMN from_location_id,
    DROP COLUMN to_location_id,
    DROP COLUMN bap_id,
    DROP COLUMN bap_uri,
    DROP COLUMN estimated_distance,
    DROP COLUMN estimated_duration,
    DROP COLUMN transaction_id,
    DROP COLUMN auto_assign_enabled,
    DROP COLUMN device;