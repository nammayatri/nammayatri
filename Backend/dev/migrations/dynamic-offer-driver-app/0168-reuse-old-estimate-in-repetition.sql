CREATE INDEX idx_driver_quote_s_try_id ON atlas_driver_offer_bpp.driver_quote USING btree (search_try_id);

CREATE INDEX idx_search_request_for_driver_s_try_id ON atlas_driver_offer_bpp.search_request_for_driver USING btree (search_try_id);

CREATE INDEX idx_search_try_s_req_id ON atlas_driver_offer_bpp.search_try USING btree (request_id);
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET request_id = id;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN transaction_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN transaction_id DROP NOT NULL;

UPDATE atlas_driver_offer_bpp.driver_flow_status AS T1 SET
  flow_status = CAST (CAST (T1.flow_status AS jsonb) || jsonb_build_object ('searchTryId', T1.flow_status->>'requestId') AS json)
  WHERE (T1.flow_status->>'status') = 'GOT_SEARCH_REQUEST';

UPDATE atlas_driver_offer_bpp.scheduler_job AS T1 SET
  job_data = CAST (CAST (T1.job_data AS jsonb) || jsonb_build_object ('searchTryId', CAST (T1.job_data AS jsonb)->>'requestId') AS json)
  WHERE T1.job_type = 'SendSearchRequestToDriver';

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET request_id = id WHERE T1.request_id IS NULL;

-- IS THIS REQUIRED?
-- INSERT INTO atlas_driver_offer_bpp.search_try (
--   id,
--   message_id,
--   estimate_id,
--   start_time,
--   valid_till,
--   customer_extra_fee,
--   status,
--   vehicle_variant,
--   search_repeat_counter,
--   created_at,
--   updated_at
--   )
--   (SELECT id,
--     message_id,
--     estimate_id,
--     start_time,
--     valid_till,
--     customer_extra_fee,
--     status,
--     vehicle_variant,
--     search_repeat_counter,
--     created_at,
--     updated_at FROM atlas_driver_offer_bpp.search_request as T1 WHERE NOT EXISTS (SELECT id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T1.id = T2.id));

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD CONSTRAINT
  driver_quote_to_search_try_fk FOREIGN KEY (search_try_id) REFERENCES atlas_driver_offer_bpp.search_try (id);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD CONSTRAINT
  search_request_for_driver_to_search_try_fk FOREIGN KEY (search_try_id) REFERENCES atlas_driver_offer_bpp.search_try (id);

ALTER TABLE atlas_driver_offer_bpp.search_try ADD CONSTRAINT
  search_step_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request (id);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN search_try_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN search_try_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN request_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
    DROP COLUMN transaction_id;

ALTER TABLE atlas_driver_offer_bpp.driver_quote
    DROP COLUMN transaction_id;
