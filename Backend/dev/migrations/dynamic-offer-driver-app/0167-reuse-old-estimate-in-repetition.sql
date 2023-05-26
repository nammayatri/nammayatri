CREATE TABLE atlas_driver_offer_bpp.search_request_2 (
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
  device character varying(255),
  created_at timestamp with time zone  NOT NULL DEFAULT now()
);
ALTER TABLE atlas_driver_offer_bpp.search_request_2 OWNER TO atlas_driver_offer_bpp_user;

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
    T1.device,
    T1.created_at
  FROM atlas_driver_offer_bpp.search_try AS T1
)
INSERT INTO atlas_driver_offer_bpp.search_request_2 (
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
  device,
  created_at)
  (SELECT * FROM SearchRequests);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN search_request_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN request_id character(36);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN search_try_id character(36);
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET request_id = search_request_id WHERE T1.created_at > now () - interval '6 hour';
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET search_try_id = search_request_id WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN search_request_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN request_id character(36);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN search_try_id character(36);
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET request_id = search_request_id WHERE T1.created_at > now () - interval '6 hour';
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET search_try_id = search_request_id WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN request_id character(36);
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET request_id = id;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN transaction_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.estimate ALTER COLUMN transaction_id DROP NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.estimate ADD COLUMN request_id character(36);
UPDATE atlas_driver_offer_bpp.estimate AS T1 SET request_id = (SELECT T2.id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T2.estimate_id = T1.id) WHERE T1.created_at > now () - interval '6 hour';

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN transaction_id DROP NOT NULL;

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

-------------------------------------------------------------------------------------------
-------------------------------AFTER_FULL_ROLL_OUT-----------------------------------------
-------------------------------------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET request_id = search_request_id WHERE T1.request_id IS NULL;
UPDATE atlas_driver_offer_bpp.driver_quote AS T1 SET search_try_id = search_request_id WHERE T1.search_try_id IS NULL;
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET request_id = search_request_id WHERE T1.request_id IS NULL;
UPDATE atlas_driver_offer_bpp.search_request_for_driver AS T1 SET search_try_id = search_request_id WHERE T1.search_try_id IS NULL;
UPDATE atlas_driver_offer_bpp.search_try AS T1 SET request_id = id WHERE T1.request_id IS NULL;
UPDATE atlas_driver_offer_bpp.estimate AS T1 SET request_id = (SELECT T2.id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T2.estimate_id = T1.id) WHERE T1.request_id IS NULL;

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
    T1.device,
    T1.created_at
  FROM atlas_driver_offer_bpp.search_try AS T1
  WHERE NOT EXISTS (SELECT id FROM atlas_driver_offer_bpp.search_try AS T2 WHERE T1.id = T2.id)
)
INSERT INTO atlas_driver_offer_bpp.search_request_2 (
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
  device,
  created_at)
  (SELECT * FROM SearchRequests);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD CONSTRAINT
  driver_quote_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request_2 (id);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD CONSTRAINT
  driver_quote_to_search_try_fk FOREIGN KEY (search_try_id) REFERENCES atlas_driver_offer_bpp.search_try (id);

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD CONSTRAINT
  search_request_for_driver_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request_2 (id);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD CONSTRAINT
  search_request_for_driver_to_search_try_fk FOREIGN KEY (search_try_id) REFERENCES atlas_driver_offer_bpp.search_try (id);

ALTER TABLE atlas_driver_offer_bpp.search_try ADD CONSTRAINT
  search_step_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request_2 (id);

ALTER TABLE atlas_driver_offer_bpp.estimate ADD CONSTRAINT
  estimate_to_search_request_fk FOREIGN KEY (request_id) REFERENCES atlas_driver_offer_bpp.search_request_2 (id);

ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN request_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_quote ALTER COLUMN search_try_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN request_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ALTER COLUMN search_try_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ALTER COLUMN request_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
    DROP COLUMN transaction_id;

ALTER TABLE atlas_driver_offer_bpp.estimate
    DROP COLUMN transaction_id;

ALTER TABLE atlas_driver_offer_bpp.driver_quote
    DROP COLUMN transaction_id;

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

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver
    DROP COLUMN search_request_id;

ALTER TABLE atlas_driver_offer_bpp.driver_quote
    DROP COLUMN search_request_id;

DROP TABLE atlas_driver_offer_bpp.search_request;