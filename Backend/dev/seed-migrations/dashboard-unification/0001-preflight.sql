-- ============================================================================
-- Phase 1 / 0001: preflight checks. READ-ONLY. All result sets must be empty
-- (or explained) before running 0002+. Runs against the BPP database with the
-- BAP export staged by 000a/000b into atlas_dashboard.bap_*.
--
-- Both sides are staged in this schema as bap_*/bpp_* tables by 000c.
-- ============================================================================

-- 0.1 Staging present and populated. The old cross-schema column-drift check
-- is gone: staging tables are created `LIKE` their BPP twin, so their columns
-- match by construction, and real drift is caught by 000a's column assertion
-- against the BAP database. What matters here is that 000b actually ran and
-- loaded every table the merge reads.
SELECT t.name AS staging_table,
       to_regclass('atlas_dashboard.bap_' || t.name) IS NOT NULL AS present
FROM (VALUES ('role'), ('person'), ('merchant'), ('merchant_access'),
             ('access_matrix'), ('entity'), ('person_capability'), ('access_audit')) AS t(name)
ORDER BY present, t.name;
-- Every row must be present = true. `transaction` is optional (see 000a).

SELECT 'role' AS staging_table, count(*) FROM atlas_dashboard.bap_role
UNION ALL SELECT 'person', count(*) FROM atlas_dashboard.bap_person
UNION ALL SELECT 'merchant', count(*) FROM atlas_dashboard.bap_merchant
UNION ALL SELECT 'merchant_access', count(*) FROM atlas_dashboard.bap_merchant_access
UNION ALL SELECT 'access_matrix', count(*) FROM atlas_dashboard.bap_access_matrix
UNION ALL SELECT 'entity', count(*) FROM atlas_dashboard.bap_entity
UNION ALL SELECT 'person_capability', count(*) FROM atlas_dashboard.bap_person_capability
UNION ALL SELECT 'access_audit', count(*) FROM atlas_dashboard.bap_access_audit;
-- Reconcile these against the counts 000b printed on load.

-- 0.2 Person id collisions across sides (would corrupt the union copy).
-- MUST be empty.
SELECT a.id FROM atlas_dashboard.bap_person a
JOIN atlas_dashboard.bpp_person b ON b.id = a.id;

-- empty

-- 0.2b Merchant id collisions across sides. NOT fatal: 0006 re-ids the BAP
-- side and 0007/0008 remap its references. Listed so the re-ids are expected.
SELECT b.id, b.short_id AS bap_short_id, p.short_id AS bpp_short_id
FROM atlas_dashboard.bap_merchant b
JOIN atlas_dashboard.bpp_merchant p ON p.id = b.id;

-- 0.3 Duplicate email_hash within a side (breaks 1:1 matching). MUST be empty
-- (was empty in the 2026-08-05 exports; re-check on the live DB).
SELECT 'bap' AS side, email_hash FROM atlas_dashboard.bap_person
WHERE email_hash IS NOT NULL GROUP BY email_hash HAVING count(*) > 1
UNION ALL
SELECT 'bpp', email_hash FROM atlas_dashboard.bpp_person
WHERE email_hash IS NOT NULL GROUP BY email_hash HAVING count(*) > 1;

-- empty result set means no duplicates. If any rows appear, they must be resolved

-- 0.4 Role-name duplicates within a side (breaks name-keyed remapping).
-- MUST be empty (role.name has a UNIQUE constraint, so this is belt+braces).
SELECT 'bap' AS side, name FROM atlas_dashboard.bap_role GROUP BY name HAVING count(*) > 1
UNION ALL
SELECT 'bpp', name FROM atlas_dashboard.bpp_role GROUP BY name HAVING count(*) > 1;

-- 0.5 Target schema must not exist yet.
SELECT nspname FROM pg_namespace WHERE nspname = 'atlas_dashboard';

-- 0.6 BAP persons whose role would be dropped (member of a role that is
-- retired AND has a same-name conflict nowhere). Expected: empty, because
-- retired roles (CUSTOMER, DRIVER) have zero members and INTERNAL_ADMIN is
-- BPP-side (remapped in 03).
SELECT a.id, a.first_name, ra.name
FROM atlas_dashboard.bap_person a
JOIN atlas_dashboard.bap_role ra ON ra.id = a.role_id
WHERE ra.name IN ('CUSTOMER', 'DRIVER', 'INTERNAL_ADMIN');
