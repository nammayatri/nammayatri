-- ============================================================================
-- Phase 1 / 0012: post-merge verification. All assertions state their expected
-- value; run after 0010 (and re-run after the cutover delta copy of 0008).
-- ============================================================================

-- 99.1 Row-count reconciliation.
SELECT 'person' AS tbl,
       (SELECT count(*) FROM atlas_dashboard.person) AS merged,
       (SELECT count(*) FROM atlas_bpp_dashboard.person)
         + (SELECT count(*) FROM atlas_bap_dashboard.person)
         - (SELECT count(*) FROM atlas_dashboard.legacy_bap_person) AS expected
UNION ALL
SELECT 'merchant',
       (SELECT count(*) FROM atlas_dashboard.merchant),
       (SELECT count(*) FROM atlas_bpp_dashboard.merchant)
         + (SELECT count(*) FROM atlas_bap_dashboard.merchant)
UNION ALL
SELECT 'merchant_access',
       (SELECT count(*) FROM atlas_dashboard.merchant_access),
       (SELECT count(*) FROM atlas_bpp_dashboard.merchant_access)
         + (SELECT count(*) FROM atlas_bap_dashboard.merchant_access)
UNION ALL
SELECT 'transaction',
       (SELECT count(*) FROM atlas_dashboard.transaction),
       (SELECT count(*) FROM atlas_bpp_dashboard.transaction)
         + (SELECT count(*) FROM atlas_bap_dashboard.transaction);
-- person: merged = expected exactly. transaction: merged may exceed expected
-- only if sources kept writing after the bulk copy (re-run 06 delta).

-- 99.2 Referential integrity (all MUST be 0).
SELECT 'person->role orphans' AS chk, count(*) FROM atlas_dashboard.person p
  WHERE p.role_id NOT IN (SELECT id FROM atlas_dashboard.role)
UNION ALL
SELECT 'merchant_access->person orphans', count(*) FROM atlas_dashboard.merchant_access ma
  WHERE ma.person_id NOT IN (SELECT id FROM atlas_dashboard.person)
UNION ALL
SELECT 'merchant_access->merchant orphans', count(*) FROM atlas_dashboard.merchant_access ma
  WHERE ma.merchant_id NOT IN (SELECT id FROM atlas_dashboard.merchant)
UNION ALL
SELECT 'access_matrix->role orphans', count(*) FROM atlas_dashboard.access_matrix m
  WHERE m.role_id NOT IN (SELECT id FROM atlas_dashboard.role)
UNION ALL
SELECT 'role_capability->capability orphans', count(*) FROM atlas_dashboard.role_capability rc
  WHERE rc.capability_id NOT IN (SELECT id FROM atlas_dashboard.capability)
UNION ALL
SELECT 'legacy map->person orphans', count(*) FROM atlas_dashboard.legacy_bap_person m
  WHERE m.person_id NOT IN (SELECT id FROM atlas_dashboard.person);

-- 99.3 Password policy: every matched person carries the BPP hash (MUST be 0).
SELECT count(*) AS matched_persons_not_carrying_bpp_hash
FROM atlas_dashboard.legacy_bap_person map
JOIN atlas_bpp_dashboard.person src ON src.id = map.person_id
JOIN atlas_dashboard.person dst ON dst.id = map.person_id
WHERE dst.password_hash IS DISTINCT FROM src.password_hash;

-- 99.4 Retired roles absent; remapped members landed (INTERNAL_ADMIN -> 0,
-- its 2 members counted under JUSPAY_ADMIN).
SELECT name FROM atlas_dashboard.role
WHERE name IN ('INTERNAL_ADMIN', 'CUSTOMER', 'DRIVER');   -- MUST be empty

SELECT r.name, count(p.id) AS members
FROM atlas_dashboard.role r LEFT JOIN atlas_dashboard.person p ON p.role_id = r.id
WHERE r.name IN ('JUSPAY_ADMIN', 'JUSPAY_OPS', 'FLEET', 'YATRI_SATHI_ADMIN')
GROUP BY r.name ORDER BY r.name;
-- Reference (2026-08-05 exports): JUSPAY_ADMIN = 121 BPP + 2 INTERNAL_ADMIN
-- + BAP-only JUSPAY_ADMINs; recompute against live data.

-- 99.5 admin_tier distribution (only DASHBOARD_ADMIN + USER at this point;
-- SUPER_ADMIN appears after the manual vault-controlled seed).
SELECT admin_tier, count(*) FROM atlas_dashboard.person GROUP BY admin_tier;

-- 99.6 registration_token empty (forced re-login at cutover). MUST be 0.
SELECT count(*) FROM atlas_dashboard.registration_token;

-- 99.7 Capability layer present.
SELECT (SELECT count(*) FROM atlas_dashboard.capability) AS capabilities,      -- 151
       (SELECT count(*) FROM atlas_dashboard.capability_endpoint) AS endpoints, -- 855
       (SELECT count(*) FROM atlas_dashboard.role_capability) AS role_caps,     -- > 0
       (SELECT count(*) FROM atlas_dashboard.person_capability) AS overrides;   -- ~ split-role grants

-- FINAL GATES (run separately):
--   * 0011-verdict-diff.sql
--   * widening-report query at the end of 0009-capability-seed.sql
