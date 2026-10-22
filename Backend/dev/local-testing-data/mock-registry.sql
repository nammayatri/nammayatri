-- Mock registry entries for local integration tests.
--
-- The ONDC ride-flow collection (dev/integration-tests/collections/OndcRideFlow) calls the BPP
-- as the ONDC workbench BAP with a placeholder signature. SignatureAuth skips verification for
-- subscribers listed in noSignatureSubscribers, but only after the registry lookup for the
-- key id succeeds, so the workbench key must exist here. Applied by the test-tool startup
-- runner together with the other local-testing-data files.
--
-- subscriber_url is the real workbench buyer endpoint: callbacks that resolve the BAP from the
-- registry (RSF on_receiver_recon) are delivered there, so the ONDC workbench sees them. The ride
-- flow is unaffected, its on_* callbacks go to the bap_uri in the request context (the local mock).
INSERT INTO atlas_registry.subscriber
  ( unique_key_id
  , subscriber_id
  , subscriber_url
  , type
  , domain
  , city
  , country
  , status
  , signing_public_key
  , encr_public_key
  , valid_from
  , valid_until
  )
SELECT
    'c11d8b66-a8fb-4962-b1d8-66a66a67079d'
  , 'workbench.ondc.tech'
  , 'https://workbench.ondc.tech/api-service/ONDC:NTS10/1.0.0/buyer'
  , 'BAP'
  , 'MOBILITY'
  , '{std:011}'
  , 'India'
  , 'SUBSCRIBED'
  , '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=' -- any valid key: the workbench is in noSignatureSubscribers
  , NULL
  , now()
  , '2031-12-31 00:00:00+00'
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_registry.subscriber s
  WHERE s.subscriber_id = 'workbench.ondc.tech'
    AND s.unique_key_id = 'c11d8b66-a8fb-4962-b1d8-66a66a67079d'
);

-- The same key is also presented in X-Gateway-Authorization (the collection has no real gateway),
-- which SignatureAuth resolves as subscriber type BG, so the key needs a BG row as well.
INSERT INTO atlas_registry.subscriber
  ( unique_key_id
  , subscriber_id
  , subscriber_url
  , type
  , domain
  , city
  , country
  , status
  , signing_public_key
  , encr_public_key
  , valid_from
  , valid_until
  )
SELECT
    'c11d8b66-a8fb-4962-b1d8-66a66a67079d'
  , 'workbench.ondc.tech'
  , 'https://workbench.ondc.tech/api-service/ONDC:NTS10/1.0.0/buyer'
  , 'BG'
  , 'MOBILITY'
  , '{std:011}'
  , 'India'
  , 'SUBSCRIBED'
  , '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws='
  , NULL
  , now()
  , '2031-12-31 00:00:00+00'
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_registry.subscriber s
  WHERE s.subscriber_id = 'workbench.ondc.tech'
    AND s.unique_key_id = 'c11d8b66-a8fb-4962-b1d8-66a66a67079d'
    AND s.type = 'BG'
);

-- Rows seeded before subscriber_url pointed at the workbench: repoint them once.
UPDATE atlas_registry.subscriber
SET subscriber_url = 'https://workbench.ondc.tech/api-service/ONDC:NTS10/1.0.0/buyer'
  , updated = now()
WHERE subscriber_id = 'workbench.ondc.tech'
  AND unique_key_id = 'c11d8b66-a8fb-4962-b1d8-66a66a67079d'
  AND subscriber_url <> 'https://workbench.ondc.tech/api-service/ONDC:NTS10/1.0.0/buyer';

-- Local RSF collector (BAP) for testing receiver_recon without the ONDC workbench. It signs with a
-- placeholder signature (exempted in noSignatureSubscribers), and since on_receiver_recon is delivered to
-- the registry's subscriber_url, the reply lands in the mock server's BAP callback store:
--   GET http://localhost:8080/workbench-bap/callbacks?txn=<transaction_id>&action=on_receiver_recon
INSERT INTO atlas_registry.subscriber
  ( unique_key_id
  , subscriber_id
  , subscriber_url
  , type
  , domain
  , city
  , country
  , status
  , signing_public_key
  , encr_public_key
  , valid_from
  , valid_until
  )
SELECT
    'local-collector-key'
  , 'local.collector'
  , 'http://localhost:8080/workbench-bap'
  , 'BAP'
  , 'MOBILITY'
  , '{std:011}'
  , 'India'
  , 'SUBSCRIBED'
  , '1tRC4LHVKWeGikY5C8T1FYI9t3oacpa3IFvj8e7aGws=' -- any valid key: local.collector is in noSignatureSubscribers
  , NULL
  , now()
  , '2031-12-31 00:00:00+00'
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_registry.subscriber s
  WHERE s.subscriber_id = 'local.collector' AND s.unique_key_id = 'local-collector-key'
);
