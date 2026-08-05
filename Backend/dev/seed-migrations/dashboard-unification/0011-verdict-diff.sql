-- ============================================================================
-- Offline verdict diff: legacy access_matrix vs capability framework.
-- The PRIMARY pre-flip gate (exhaustive, unlike runtime shadow which only
-- covers endpoints that receive traffic). Run per schema after applying the
-- capability seed; replace atlas_dashboard. Read-only.
--
-- FLIP SEMANTICS (decided 2026-08-06): enforcement = capability OR legacy
-- access_matrix row. The matrix survives as a TRANSITIONAL fallback, so no
-- one loses access at flip time; it is narrowed role-by-role afterwards.
--
-- Goal state before the flip:
--   Section 1 (fallback-dependency) — NOT breakage under OR semantics. It is
--     the tightening worklist: role/endpoint pairs still reachable only via
--     the matrix. Drive toward zero before deleting access_matrix (Phase 7).
--   Section 2 (unmapped) — legacy pre-DSL rows (DRIVERS/LIST etc.) + stale
--     ids. Harmless (no live route resolves to them); purged by the merge.
--   Section 3 (widening) — the REVIEWABLE direction: access nobody had
--     before. Threshold derivation (50% coverage) keeps this small; the
--     remainder must be accepted role-by-role, high member counts first.
-- ============================================================================

-- 1. FALLBACK DEPENDENCY: legacy allows, capability model does not.
--    Under OR semantics these keep working via the matrix — this is the
--    tightening worklist, ordered by blast radius.
SELECT r.name              AS role_name,
       m.user_action_type  AS endpoint_id,
       ce.capability_id,
       (SELECT count(*) FROM atlas_dashboard.person p WHERE p.role_id = r.id) AS members
FROM atlas_dashboard.access_matrix m
JOIN atlas_dashboard.role r  ON r.id = m.role_id
JOIN atlas_dashboard.capability_endpoint ce ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES' THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type ELSE m.user_action_type END
LEFT JOIN atlas_dashboard.role_capability rc
       ON rc.role_id = m.role_id AND rc.capability_id = ce.capability_id
WHERE m.user_access_type = 'USER_FULL_ACCESS'
  AND rc.role_id IS NULL
ORDER BY members DESC, r.name, ce.capability_id;

-- 2. UNMAPPED: matrix rows whose action has no capability_endpoint entry.
--    Expected: the 13 stale ids (renamed/deleted endpoints), 12 BHARAT_TAXI_*,
--    and unmounted actions. Anything else = seed generator gap — fix before flip.
SELECT DISTINCT m.user_action_type AS endpoint_id,
       count(DISTINCT m.role_id)   AS roles_holding
FROM atlas_dashboard.access_matrix m
LEFT JOIN atlas_dashboard.capability_endpoint ce ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES' THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type ELSE m.user_action_type END
WHERE m.user_access_type = 'USER_FULL_ACCESS'
  AND ce.endpoint_id IS NULL
GROUP BY m.user_action_type
ORDER BY roles_holding DESC;

-- 3. WIDENING: capability model allows, legacy never did.
--    (Role gained the capability via another endpoint in the same bucket.)
SELECT r.name             AS role_name,
       rc.capability_id,
       ce.endpoint_id     AS gained_endpoint,
       (SELECT count(*) FROM atlas_dashboard.person p WHERE p.role_id = r.id) AS members
FROM atlas_dashboard.role_capability rc
JOIN atlas_dashboard.role r ON r.id = rc.role_id
JOIN atlas_dashboard.capability_endpoint ce ON ce.capability_id = rc.capability_id
LEFT JOIN atlas_dashboard.access_matrix m
       ON m.role_id = rc.role_id
      AND ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES' THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type ELSE m.user_action_type END
      AND m.user_access_type = 'USER_FULL_ACCESS'
WHERE m.id IS NULL
ORDER BY members DESC, r.name, rc.capability_id, ce.endpoint_id;

-- 4. Summary counts per direction (quick health read).
SELECT 'fallback_dependency' AS direction, count(*) FROM (
  SELECT 1 FROM atlas_dashboard.access_matrix m
  JOIN atlas_dashboard.capability_endpoint ce ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES' THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type ELSE m.user_action_type END
  LEFT JOIN atlas_dashboard.role_capability rc
         ON rc.role_id = m.role_id AND rc.capability_id = ce.capability_id
  WHERE m.user_access_type = 'USER_FULL_ACCESS' AND rc.role_id IS NULL) x
UNION ALL
SELECT 'unmapped', count(DISTINCT m.user_action_type)
FROM atlas_dashboard.access_matrix m
LEFT JOIN atlas_dashboard.capability_endpoint ce ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES' THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type ELSE m.user_action_type END
WHERE m.user_access_type = 'USER_FULL_ACCESS' AND ce.endpoint_id IS NULL
UNION ALL
SELECT 'widening', count(*) FROM (
  SELECT 1 FROM atlas_dashboard.role_capability rc
  JOIN atlas_dashboard.capability_endpoint ce ON ce.capability_id = rc.capability_id
  LEFT JOIN atlas_dashboard.access_matrix m
         ON m.role_id = rc.role_id AND ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES' THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type ELSE m.user_action_type END
        AND m.user_access_type = 'USER_FULL_ACCESS'
  WHERE m.id IS NULL) y;
