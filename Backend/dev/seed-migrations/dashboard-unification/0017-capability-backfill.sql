\set ON_ERROR_STOP on

-- ===========================================================================
-- 0016 — make the capability set a SUPERSET of the legacy access matrix.
--
-- Prerequisite for removing the `capability OR legacy` fallback from
-- Tools.Auth.Api.verifyAccessLevel. Until this runs, the capability set is a
-- SUBSET of legacy: 0009's threshold derivation grants a role a capability
-- only at >= 50%% endpoint coverage, deliberately leaving the sub-threshold
-- tail to the fallback. Delete the fallback without running this and every
-- sub-threshold holding becomes a 403.
--
-- This is the same derivation with the cutoff dropped to "holds >= 1 endpoint".
--
-- THE TRADEOFF, STATED PLAINLY: capability is a coarser unit than the matrix.
-- A role holding 1 of a capability's 10 endpoints now gets all 10. That is
-- widening, and it is the deliberate choice — per PLAN.md, "give access what
-- they have, we will limit later". Section 3 quantifies the blast radius
-- before you commit; section 4 is the trimming worklist for afterwards.
--
-- Run on atlas_dashboard AFTER 0009/0010, and BEFORE deploying the binary that
-- drops the fallback.
-- ===========================================================================

-- ---------------------------------------------------------------------------
-- 1. FAIL-LOUD: matrix endpoints with no capability behind them.
--
-- These are granted today via the matrix and map to NOTHING in
-- capability_endpoint, so after the fallback is removed they become
-- permanently unreachable regardless of what anyone is granted. This must be
-- empty before you deploy. If it is not, extend the endpoint shim in
-- generate_capability_seed.py and regenerate 0009 — do not hand-patch.
SELECT DISTINCT
       m.api_entity,
       m.user_action_type,
       count(DISTINCT m.role_id) AS roles_affected
FROM atlas_dashboard.access_matrix m
WHERE m.user_access_type = 'USER_FULL_ACCESS'
  AND NOT EXISTS (
      SELECT 1 FROM atlas_dashboard.capability_endpoint ce
      WHERE ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
             THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
             ELSE m.user_action_type END)
GROUP BY m.api_entity, m.user_action_type
ORDER BY roles_affected DESC;

-- ---------------------------------------------------------------------------
-- 2. The backfill itself: grant the capability behind every endpoint the role
-- already holds in the matrix. Same shape as 0009's derivation, no threshold.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT DISTINCT m.role_id, ce.capability_id
FROM atlas_dashboard.access_matrix m
JOIN atlas_dashboard.capability_endpoint ce
  ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
     THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
     ELSE m.user_action_type END
WHERE m.user_access_type = 'USER_FULL_ACCESS'
ON CONFLICT DO NOTHING;

-- ---------------------------------------------------------------------------
-- 3. Blast radius: capabilities gained per role, and how thin the evidence was.
-- `pct_held` is the share of the capability's endpoints the role actually held
-- in the matrix — a low number means this grant widened the role a lot. Read
-- this together with member counts before deploying.
SELECT r.name AS role_name,
       (SELECT count(*) FROM atlas_dashboard.person p WHERE p.role_id = r.id) AS members,
       ce.capability_id,
       count(DISTINCT ce.endpoint_id) AS endpoints_held,
       sized.total_endpoints,
       round(100.0 * count(DISTINCT ce.endpoint_id) / sized.total_endpoints) AS pct_held
FROM atlas_dashboard.access_matrix m
JOIN atlas_dashboard.capability_endpoint ce
  ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
     THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
     ELSE m.user_action_type END
JOIN atlas_dashboard.role r ON r.id = m.role_id
JOIN (SELECT capability_id, count(*) AS total_endpoints
      FROM atlas_dashboard.capability_endpoint GROUP BY capability_id) sized
  ON sized.capability_id = ce.capability_id
WHERE m.user_access_type = 'USER_FULL_ACCESS'
GROUP BY r.id, r.name, ce.capability_id, sized.total_endpoints
HAVING count(DISTINCT ce.endpoint_id) < sized.total_endpoints
ORDER BY members DESC, pct_held ASC;

-- ---------------------------------------------------------------------------
-- 4. Trimming worklist (run later, not now): sensitive capabilities a role
-- picked up on thin evidence. Trim with a role_capability DELETE, or a
-- person_capability DENY for individuals.
SELECT r.name AS role_name, ce.capability_id,
       count(DISTINCT ce.endpoint_id) || '/' || sized.total_endpoints AS held
FROM atlas_dashboard.access_matrix m
JOIN atlas_dashboard.capability_endpoint ce
  ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
     THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
     ELSE m.user_action_type END
JOIN atlas_dashboard.role r ON r.id = m.role_id
JOIN (SELECT capability_id, count(*) AS total_endpoints
      FROM atlas_dashboard.capability_endpoint GROUP BY capability_id) sized
  ON sized.capability_id = ce.capability_id
WHERE m.user_access_type = 'USER_FULL_ACCESS'
  AND (ce.capability_id LIKE '%.write' OR ce.capability_id LIKE '%.approve'
       OR ce.capability_id LIKE '%.execute' OR ce.capability_id LIKE '%.export'
       OR ce.capability_id LIKE 'admin.%' OR ce.capability_id LIKE 'finance.%')
GROUP BY r.id, r.name, ce.capability_id, sized.total_endpoints
HAVING count(DISTINCT ce.endpoint_id)::numeric / sized.total_endpoints < 0.5
ORDER BY r.name, ce.capability_id;

-- ---------------------------------------------------------------------------
-- 5. Re-apply 0009's deliberate DASHBOARD_OPERATOR revocation, which section 2
-- will have just undone.
--
-- NOTE — this changes meaning once the fallback is gone. In 0009 this DELETE
-- only hid pages in the UI; the matrix still served the API. Now it denies at
-- the API too. That matches the original intent (operators shared the fleet
-- login and were never meant to see Finances or Settings), but it IS an API
-- access change for that role. Comment this block out if you would rather keep
-- operators whole for now and trim later.
DELETE FROM atlas_dashboard.role_capability rc
USING atlas_dashboard.role r
WHERE rc.role_id = r.id
  AND r.name = 'DASHBOARD_OPERATOR'
  AND rc.capability_id IN ('fleet.earnings.read', 'fleet.onboarding.read', 'fleet.onboarding.write');

-- ---------------------------------------------------------------------------
-- 6. Gate before deploying the no-fallback binary. Both must read 0.
SELECT 'matrix grants with no capability behind them' AS check_name, count(*) AS value
FROM (
  SELECT DISTINCT m.role_id, m.api_entity, m.user_action_type
  FROM atlas_dashboard.access_matrix m
  WHERE m.user_access_type = 'USER_FULL_ACCESS'
    AND NOT EXISTS (
        SELECT 1 FROM atlas_dashboard.capability_endpoint ce
        WHERE ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
               THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
               ELSE m.user_action_type END)
) x
UNION ALL
SELECT 'role/endpoint pairs allowed by matrix but not by capability', count(*)
FROM (
  SELECT DISTINCT m.role_id, ce.capability_id
  FROM atlas_dashboard.access_matrix m
  JOIN atlas_dashboard.capability_endpoint ce
    ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
       THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
       ELSE m.user_action_type END
  JOIN atlas_dashboard.role r ON r.id = m.role_id
  WHERE m.user_access_type = 'USER_FULL_ACCESS'
    AND NOT EXISTS (SELECT 1 FROM atlas_dashboard.role_capability rc
                    WHERE rc.role_id = m.role_id AND rc.capability_id = ce.capability_id)
    -- section 5's deliberate revocation is the one expected shortfall
    AND NOT (r.name = 'DASHBOARD_OPERATOR'
             AND ce.capability_id IN ('fleet.earnings.read', 'fleet.onboarding.read', 'fleet.onboarding.write'))
) y;
