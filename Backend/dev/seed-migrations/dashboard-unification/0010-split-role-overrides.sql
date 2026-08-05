-- ============================================================================
-- Phase 1 / 0010: split-role person_capability overrides.
--
-- PREREQUISITES: 0003 (capability DDL) and 0009 (capability seed) already ran
-- against atlas_dashboard. The derived part of 0009 runs against the MERGED
-- access_matrix, so a shared-name role's bundle automatically unions both
-- sides' grants.
-- ============================================================================

-- 7.1 Split-role deltas (221 persons in the 2026-08-05 exports): the merged
-- person kept their BPP role; grant the capabilities their BAP role's bundle
-- has beyond it, as person_capability GRANT rows. Sensitive capabilities are
-- excluded — same list as the derived seed; they only move via curation.
INSERT INTO atlas_dashboard.person_capability
  (person_id, capability_id, mode, reason, granted_by, expires_at)
SELECT DISTINCT
       map.person_id,
       rc_bap.capability_id,
       'GRANT',
       'merge 2026: union of BAP-side role ' || ra.name,
       NULL, NULL
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_bap_dashboard.person a   ON a.id = map.bap_person_id
JOIN atlas_bap_dashboard.role ra    ON ra.id = a.role_id
JOIN atlas_dashboard.person p       ON p.id = map.person_id
JOIN atlas_dashboard.role rb        ON rb.id = p.role_id
JOIN atlas_dashboard.role r_bap     ON r_bap.name = ra.name       -- BAP role in merged set
JOIN atlas_dashboard.role_capability rc_bap ON rc_bap.role_id = r_bap.id
WHERE ra.name <> rb.name
  AND rc_bap.capability_id NOT IN (
    'ops.pii.read', 'finance.adjustment.write', 'system.query.execute',
    'system.crypto.execute', 'config.scheduler.execute', 'config.failover.execute',
    'config.fare_policy.export', 'access.user.write', 'access.role.write',
    'access.capability.grant', 'access.admin.write', 'access.merchant.write')
  AND NOT EXISTS (   -- skip what the primary (BPP) role already grants
    SELECT 1 FROM atlas_dashboard.role_capability rc_own
    WHERE rc_own.role_id = rb.id AND rc_own.capability_id = rc_bap.capability_id)
ON CONFLICT DO NOTHING;

-- 7.2 Report: overrides created per person (review; feeds the drift report —
-- a recurring pattern here means a real role is missing).
SELECT p.first_name, p.last_name, rb.name AS primary_role,
       count(*) AS granted_caps,
       array_agg(pc.capability_id ORDER BY pc.capability_id) AS caps
FROM atlas_dashboard.person_capability pc
JOIN atlas_dashboard.person p ON p.id = pc.person_id
JOIN atlas_dashboard.role rb ON rb.id = p.role_id
WHERE pc.reason LIKE 'merge 2026:%'
GROUP BY p.id, p.first_name, p.last_name, rb.name
ORDER BY granted_caps DESC;

-- 7.3 Sensitive-cap losses from the split-role rule (BAP role had a sensitive
-- cap the BPP primary lacks — deliberately NOT auto-granted). Review this
-- list; genuine needs get explicit person_capability GRANTs with a real
-- reason, by an admin, post-merge.
SELECT DISTINCT p.first_name, p.last_name, ra.name AS bap_role, rb.name AS primary_role,
       rc_bap.capability_id AS not_granted_sensitive_cap
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_bap_dashboard.person a   ON a.id = map.bap_person_id
JOIN atlas_bap_dashboard.role ra    ON ra.id = a.role_id
JOIN atlas_dashboard.person p       ON p.id = map.person_id
JOIN atlas_dashboard.role rb        ON rb.id = p.role_id
JOIN atlas_dashboard.role r_bap     ON r_bap.name = ra.name
JOIN atlas_dashboard.role_capability rc_bap ON rc_bap.role_id = r_bap.id
WHERE ra.name <> rb.name
  AND rc_bap.capability_id IN (
    'ops.pii.read', 'finance.adjustment.write', 'system.query.execute',
    'system.crypto.execute', 'config.scheduler.execute', 'config.failover.execute',
    'config.fare_policy.export', 'access.user.write', 'access.role.write',
    'access.capability.grant', 'access.admin.write', 'access.merchant.write')
  AND NOT EXISTS (
    SELECT 1 FROM atlas_dashboard.role_capability rc_own
    WHERE rc_own.role_id = rb.id AND rc_own.capability_id = rc_bap.capability_id)
ORDER BY p.first_name;

-- NEXT: 0011-verdict-diff.sql — the primary pre-flip gate.
