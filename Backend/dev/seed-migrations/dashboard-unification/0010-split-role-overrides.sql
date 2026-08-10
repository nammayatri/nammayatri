-- ============================================================================
-- Phase 1 / 0010: split-role person_capability overrides.
--
-- PREREQUISITES: 0003 (capability DDL) and 0009 (capability seed) already ran
-- against atlas_dashboard. The derived part of 0009 runs against the MERGED
-- access_matrix, so a shared-name role's bundle automatically unions both
-- sides' grants.
-- ============================================================================

-- 7.0 Carry across EXISTING admin-created overrides from both sides. These are
-- real decisions someone made in the Access Control UI and are not regenerable
-- (unlike capability / capability_endpoint / role_capability, which 0009
-- rebuilds from the merged access_matrix). Runs BEFORE 7.1 so an explicit
-- override always wins over a generated merge grant: 7.1 uses
-- ON CONFLICT DO NOTHING against the same (person_id, capability_id) key.
INSERT INTO atlas_dashboard.person_capability
  (person_id, capability_id, mode, reason, granted_by, expires_at, created_at)
SELECT pc.person_id, pc.capability_id, pc.mode, pc.reason, pc.granted_by, pc.expires_at, pc.created_at
FROM atlas_dashboard.bpp_person_capability pc
WHERE pc.person_id IN (SELECT id FROM atlas_dashboard.person)
  AND pc.capability_id IN (SELECT id FROM atlas_dashboard.capability)
ON CONFLICT DO NOTHING;

INSERT INTO atlas_dashboard.person_capability
  (person_id, capability_id, mode, reason, granted_by, expires_at, created_at)
SELECT coalesce(map.person_id, pc.person_id), pc.capability_id, pc.mode, pc.reason,
       pc.granted_by, pc.expires_at, pc.created_at
FROM atlas_dashboard.bap_person_capability pc
LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = pc.person_id
WHERE coalesce(map.person_id, pc.person_id) IN (SELECT id FROM atlas_dashboard.person)
  AND pc.capability_id IN (SELECT id FROM atlas_dashboard.capability)
ON CONFLICT DO NOTHING;

-- Overrides dropped because their capability id no longer exists (e.g. renamed
-- during curation) or their person did not survive. Review and re-issue.
SELECT 'bpp' AS side, pc.person_id, pc.capability_id, pc.mode, pc.reason
FROM atlas_dashboard.bpp_person_capability pc
WHERE pc.capability_id NOT IN (SELECT id FROM atlas_dashboard.capability)
   OR pc.person_id NOT IN (SELECT id FROM atlas_dashboard.person)
UNION ALL
SELECT 'bap', pc.person_id, pc.capability_id, pc.mode, pc.reason
FROM atlas_dashboard.bap_person_capability pc
LEFT JOIN atlas_dashboard.legacy_bap_person map ON map.bap_person_id = pc.person_id
WHERE pc.capability_id NOT IN (SELECT id FROM atlas_dashboard.capability)
   OR coalesce(map.person_id, pc.person_id) NOT IN (SELECT id FROM atlas_dashboard.person);

-- 7.1 Split-role deltas (221 persons in the 2026-08-05 exports): the merged
-- person kept their BPP role; grant the capabilities their BAP role's bundle
-- has beyond it, as person_capability GRANT rows. NOTHING is withheld: the
-- person already holds these on the BAP side today, so filtering "sensitive"
-- ones out would REMOVE access the merge is supposed to preserve. Tightening
-- happens later, from the curation worksheet and shadow-log usage data.
INSERT INTO atlas_dashboard.person_capability
  (person_id, capability_id, mode, reason, granted_by, expires_at)
SELECT DISTINCT
       map.person_id,
       rc_bap.capability_id,
       'GRANT',
       'merge 2026: union of BAP-side role ' || ra.name,
       -- bare NULLs in a SELECT list are typed `text`; cast to the column types
       NULL::character(36),               -- granted_by: generated, not a person
       NULL::timestamp with time zone     -- expires_at: no expiry
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_dashboard.bap_person a   ON a.id = map.bap_person_id
JOIN atlas_dashboard.bap_role ra    ON ra.id = a.role_id
JOIN atlas_dashboard.person p       ON p.id = map.person_id
JOIN atlas_dashboard.role rb        ON rb.id = p.role_id
JOIN atlas_dashboard.role r_bap     ON r_bap.name = ra.name       -- BAP role in merged set
JOIN atlas_dashboard.role_capability rc_bap ON rc_bap.role_id = r_bap.id
WHERE ra.name <> rb.name
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

-- (7.3 removed: it listed sensitive capabilities deliberately withheld from
-- split-role persons. Nothing is withheld any more — see 7.1.)

-- NEXT: 0011-verdict-diff.sql — the primary pre-flip gate.
