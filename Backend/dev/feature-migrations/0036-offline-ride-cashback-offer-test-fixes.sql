-- DB-only fixes required for the "Offline Ride Booking Offers (10% First Ride
-- Cashback)" integration test (Local_BT_Delhi) to pass. These patch gaps in
-- the local config-sync data source, not application bugs.

-- 1. Bharat Taxi Partner's Delhi subscriber row had status NULL in the
--    registry, so it never participated in Delhi's ONDC search broadcast
--    (no on_search response, empty estimates).
UPDATE atlas_registry.subscriber
SET status = 'SUBSCRIBED', updated = now()
WHERE subscriber_id = 'localhost:8016/beckn/7e3d0f76-4c63-4b38-93c4-29b1e508fa21';

-- 2. Bharat Taxi Partner's local BPP endpoint wasn't in the BAP's whitelist,
--    so on_search callbacks were rejected with
--    "E400 INVALID_REQUEST: It is not a whitelisted subscriber ...".
--    white_list_org has no unique constraint besides its (random) id, so this
--    uses WHERE NOT EXISTS rather than ON CONFLICT to stay idempotent.
INSERT INTO atlas_app.white_list_org (id, subscriber_id, domain, merchant_id, merchant_operating_city_id, created_at, updated_at)
SELECT gen_random_uuid()::text, v.subscriber_id, v.domain, v.merchant_id, v.merchant_operating_city_id, now(), now()
FROM (
  VALUES
    ('localhost:8016/beckn/7e3d0f76-4c63-4b38-93c4-29b1e508fa21', 'MOBILITY', 'c91a2f44-3a7e-4e86-8c1e-2c4f3b0b4c98', '1e84858a-4deb-45d6-a97b-7c7504358548'),
    ('localhost:8016/beckn/7e3d0f76-4c63-4b38-93c4-29b1e508fa21', 'MOBILITY', '4b17bd06-ae7e-48e9-85bf-282fb310209c', '307c445a-2200-c45f-8d67-6f6dbfa28e73')
) AS v(subscriber_id, domain, merchant_id, merchant_operating_city_id)
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_app.white_list_org w
  WHERE w.subscriber_id = v.subscriber_id
    AND w.merchant_id = v.merchant_id
    AND w.merchant_operating_city_id = v.merchant_operating_city_id
);

-- 3. CUMULATIVE-OFFER-POLICY dynamic logic (the JSON-logic script that decides
--    which cashback offer applies) was only rolled out for a specific
--    merchant/city, not the one this test actually runs against. Reuses the
--    existing version=1 script (see app_dynamic_logic_element) via the
--    merchant_operating_city_id='default' fallback that
--    selectAppDynamicLogicVersion (Tools/DynamicLogic.hs) falls back to when
--    no city-specific rollout row is found.
INSERT INTO atlas_app.app_dynamic_logic_rollout
  (domain, merchant_operating_city_id, percentage_rollout, time_bounds, version, version_description, merchant_id, created_at, updated_at)
VALUES
  ('CUMULATIVE-OFFER-POLICY', 'default', 100, 'Unbounded', 1, 'First Ride Cashback (default fallback)', '4b17bd06-ae7e-48e9-85bf-282fb310209c', now(), now())
ON CONFLICT DO NOTHING;
