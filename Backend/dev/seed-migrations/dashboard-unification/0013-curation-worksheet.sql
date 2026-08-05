-- ============================================================================
-- 0013: curation worksheet (read-only). The manual-review input that squeezes
-- widening out for the roles that matter: on the 2026-08-06 prod copy, 18 BPP
-- roles (97% of users) and 13 BAP roles (92%) have >5 members.
--
-- Workflow per role: read A (what it gains), then B (what still needs the
-- legacy fallback). Fold accepted grants into the CURATED bundle in
-- docs/access-unification/capability-seed.md §4, regenerate, re-run 0011.
-- Replace {schema} per side (pre-merge) or use atlas_dashboard (post-merge).
-- ============================================================================

\echo '===== A. WIDENING: access these roles would GAIN (review + accept/trim) ====='
WITH members AS (
  SELECT r.id, r.name, count(p.id) AS n
  FROM {schema}.role r LEFT JOIN {schema}.person p ON p.role_id = r.id
  GROUP BY r.id, r.name)
SELECT m.name AS role_name, m.n AS members, rc.capability_id,
       count(*) AS endpoints_gained
FROM {schema}.role_capability rc
JOIN members m ON m.id = rc.role_id
JOIN {schema}.capability_endpoint ce ON ce.capability_id = rc.capability_id
LEFT JOIN {schema}.access_matrix am ON am.role_id = rc.role_id
      AND ce.endpoint_id = CASE WHEN am.api_entity = 'SPECIAL_ZONES'
            THEN 'LEGACY/SPECIAL_ZONES/' || am.user_action_type
            ELSE am.user_action_type END
      AND am.user_access_type = 'USER_FULL_ACCESS'
WHERE am.id IS NULL AND m.n > 5
GROUP BY m.name, m.n, rc.capability_id
ORDER BY m.n DESC, endpoints_gained DESC;

\echo ''
\echo '===== B. FALLBACK DEPENDENCY: held today, below the 50% derive threshold ====='
WITH members AS (
  SELECT r.id, r.name, count(p.id) AS n
  FROM {schema}.role r LEFT JOIN {schema}.person p ON p.role_id = r.id
  GROUP BY r.id, r.name),
held AS (
  SELECT am.role_id, ce.capability_id, count(DISTINCT ce.endpoint_id) AS held_endpoints
  FROM {schema}.access_matrix am
  JOIN {schema}.capability_endpoint ce
    ON ce.endpoint_id = CASE WHEN am.api_entity = 'SPECIAL_ZONES'
         THEN 'LEGACY/SPECIAL_ZONES/' || am.user_action_type
         ELSE am.user_action_type END
  WHERE am.user_access_type = 'USER_FULL_ACCESS'
  GROUP BY am.role_id, ce.capability_id),
sized AS (
  SELECT capability_id, count(*) AS total_endpoints
  FROM {schema}.capability_endpoint GROUP BY capability_id)
SELECT m.name AS role_name, m.n AS members, h.capability_id,
       h.held_endpoints, s.total_endpoints,
       round(100.0 * h.held_endpoints / s.total_endpoints) AS pct_held
FROM held h
JOIN sized s USING (capability_id)
JOIN members m ON m.id = h.role_id
LEFT JOIN {schema}.role_capability rc
       ON rc.role_id = h.role_id AND rc.capability_id = h.capability_id
WHERE rc.role_id IS NULL AND m.n > 5
ORDER BY m.n DESC, pct_held DESC;
