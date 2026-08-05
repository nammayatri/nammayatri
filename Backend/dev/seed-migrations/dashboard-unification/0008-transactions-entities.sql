-- ============================================================================
-- Phase 1 / 0008: audit log + entities.
-- IDEMPOTENT (WHERE NOT EXISTS by id) so it can re-run as a delta copy during
-- the cutover window: run once ahead of time (bulk), then again just before
-- flipping traffic to pick up rows written since.
-- ============================================================================

-- 6.1 BPP transactions (requestor ids already canonical).
INSERT INTO atlas_dashboard.transaction
  (id, requestor_id, server_name, merchant_id, endpoint,
   common_driver_id, common_ride_id, request, response, response_error, created_at)
SELECT t.id, t.requestor_id, t.server_name, t.merchant_id, t.endpoint,
       t.common_driver_id, t.common_ride_id, t.request, t.response,
       t.response_error, t.created_at
FROM atlas_bpp_dashboard.transaction t
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.transaction d WHERE d.id = t.id);

-- 6.2 BAP transactions, requestor remapped for matched persons. Requestors
-- that never became persons in the merged store (deleted accounts referenced
-- by old audit rows) are kept as-is — transaction has no FK on requestor
-- (dropped in ddl 0083), and legacy_bap_person preserves traceability.
INSERT INTO atlas_dashboard.transaction
  (id, requestor_id, server_name, merchant_id, endpoint,
   common_driver_id, common_ride_id, request, response, response_error, created_at)
SELECT t.id, coalesce(map.person_id, t.requestor_id),
       t.server_name, t.merchant_id, t.endpoint,
       t.common_driver_id, t.common_ride_id, t.request, t.response,
       t.response_error, t.created_at
FROM atlas_bap_dashboard.transaction t
LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = t.requestor_id
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.transaction d WHERE d.id = t.id);

-- 6.3 Entities (BAP is the live source; BPP-side table exists via lib but is
-- unioned defensively). SELECT * is safe here: the merged table shape was
-- cloned from atlas_bap_dashboard in 0002, and preflight 0.1 (0001) verified parity.
INSERT INTO atlas_dashboard.entity
SELECT e.* FROM atlas_bap_dashboard.entity e
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.entity d WHERE d.id = e.id);

INSERT INTO atlas_dashboard.entity
SELECT e.* FROM atlas_bpp_dashboard.entity e
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.entity d WHERE d.id = e.id);

-- registration_token: intentionally NOT copied. Cutover forces re-login,
-- which is also the token-invalidation story (PLAN.md Phase 1).
