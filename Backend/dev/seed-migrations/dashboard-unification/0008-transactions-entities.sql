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
FROM atlas_dashboard.bpp_transaction t
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.transaction d WHERE d.id = t.id);

-- 6.2 BAP transactions, requestor remapped for matched persons.
-- No-op unless the BAP `transaction` lines were uncommented in 000a/000c. Requestors
-- that never became persons in the merged store (deleted accounts referenced
-- by old audit rows) are kept as-is — transaction has no FK on requestor
-- (dropped in ddl 0083), and legacy_bap_person preserves traceability.
INSERT INTO atlas_dashboard.transaction
  (id, requestor_id, server_name, merchant_id, endpoint,
   common_driver_id, common_ride_id, request, response, response_error, created_at)
SELECT t.id, coalesce(map.person_id, t.requestor_id),
       t.server_name, coalesce(mm.merchant_id, t.merchant_id), t.endpoint,
       t.common_driver_id, t.common_ride_id, t.request, t.response,
       t.response_error, t.created_at
FROM atlas_dashboard.bap_transaction t
LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = t.requestor_id
LEFT JOIN atlas_dashboard.legacy_bap_merchant mm ON mm.bap_merchant_id = t.merchant_id
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.transaction d WHERE d.id = t.id);

-- 6.3 Entities (BAP is the live source; BPP-side table exists via lib but is
-- unioned defensively). SELECT * is safe here: the merged table shape was
-- the same shape as the staged tables it is loaded from.
-- entity.merchant_id follows the re-ided merchants too, so the columns are
-- listed explicitly rather than SELECT *.
INSERT INTO atlas_dashboard.entity
  (id, merchant_id, entity_name, entity_short_id, deleted, created_at, updated_at)
SELECT e.id, coalesce(mm.merchant_id, e.merchant_id), e.entity_name,
       e.entity_short_id, e.deleted, e.created_at, e.updated_at
FROM atlas_dashboard.bap_entity e
LEFT JOIN atlas_dashboard.legacy_bap_merchant mm ON mm.bap_merchant_id = e.merchant_id
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.entity d WHERE d.id = e.id);

-- The BPP schema may not have an `entity` table at all: lib-dashboard defines
-- the type and provider-dashboard carries a HasSchemaName instance to satisfy
-- shared Beam constraints, but the table was never created there. Copy it only
-- if it exists, so this script runs on both shapes.
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables
             WHERE table_schema = 'atlas_bpp_dashboard' AND table_name = 'entity') THEN
    INSERT INTO atlas_dashboard.entity
    SELECT e.* FROM atlas_dashboard.bpp_entity e
    WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.entity d WHERE d.id = e.id);
  ELSE
    RAISE NOTICE 'atlas_dashboard.bpp_entity absent - BAP entities only (expected)';
  END IF;
END $$;

-- 6.4 Access-control audit trail. Same idempotent pattern; BAP actor ids are
-- remapped to the canonical person. Losing this would defeat the point of
-- having an audit log across the migration.
INSERT INTO atlas_dashboard.access_audit
  (id, actor_id, action, target_type, target_id, before_value, after_value, reason, created_at)
SELECT a.id, a.actor_id, a.action, a.target_type, a.target_id,
       a.before_value, a.after_value, a.reason, a.created_at
FROM atlas_dashboard.bpp_access_audit a
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.access_audit d WHERE d.id = a.id);

INSERT INTO atlas_dashboard.access_audit
  (id, actor_id, action, target_type, target_id, before_value, after_value, reason, created_at)
SELECT a.id, coalesce(map.person_id, a.actor_id), a.action, a.target_type, a.target_id,
       a.before_value, a.after_value, a.reason, a.created_at
FROM atlas_dashboard.bap_access_audit a
LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = a.actor_id
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.access_audit d WHERE d.id = a.id);

-- registration_token: intentionally NOT copied. Cutover forces re-login,
-- which is also the token-invalidation story (PLAN.md Phase 1).
