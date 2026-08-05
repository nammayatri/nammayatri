-- ============================================================================
-- Phase 1 / 0001: preflight checks. READ-ONLY. All result sets must be empty
-- (or explained) before running 01+. Assumes atlas_bap_dashboard and
-- atlas_bpp_dashboard are visible from one connection (same DB, or FDW —
-- see 0000-phase0-parity.sql header for the FDW recipe).
-- ============================================================================

-- 0.1 Column drift between the two source schemas for every table we copy.
-- The copy scripts use explicit column lists derived from the BPP shape;
-- any row here means a script needs adjusting first.
SELECT coalesce(b.table_name, p.table_name) AS table_name,
       coalesce(b.column_name, p.column_name) AS column_name,
       CASE WHEN p.column_name IS NULL THEN 'bap_only'
            WHEN b.column_name IS NULL THEN 'bpp_only'
            WHEN b.data_type <> p.data_type THEN 'TYPE DIFFERS: bap=' || b.data_type || ' bpp=' || p.data_type
       END AS issue
FROM (SELECT table_name, column_name, data_type FROM information_schema.columns
      WHERE table_schema = 'atlas_bap_dashboard'
        AND table_name IN ('person','role','access_matrix','merchant','merchant_access','transaction','entity')) b
FULL OUTER JOIN
     (SELECT table_name, column_name, data_type FROM information_schema.columns
      WHERE table_schema = 'atlas_bpp_dashboard'
        AND table_name IN ('person','role','access_matrix','merchant','merchant_access','transaction','entity')) p
  USING (table_name, column_name)
WHERE p.column_name IS NULL OR b.column_name IS NULL OR b.data_type <> p.data_type
ORDER BY table_name, column_name;
-- Expected rows: merchant bpp_only company_name/email_hash/password_hash/
-- email_encrypted (handled by 04's narrower BAP column list). Anything else: stop.

-- 0.2 Person id collisions across sides (would corrupt the union copy).
-- MUST be empty.
SELECT a.id FROM atlas_bap_dashboard.person a
JOIN atlas_bpp_dashboard.person b ON b.id = a.id;

-- 0.3 Duplicate email_hash within a side (breaks 1:1 matching). MUST be empty
-- (was empty in the 2026-08-05 exports; re-check on the live DB).
SELECT 'bap' AS side, email_hash FROM atlas_bap_dashboard.person
WHERE email_hash IS NOT NULL GROUP BY email_hash HAVING count(*) > 1
UNION ALL
SELECT 'bpp', email_hash FROM atlas_bpp_dashboard.person
WHERE email_hash IS NOT NULL GROUP BY email_hash HAVING count(*) > 1;

-- 0.4 Role-name duplicates within a side (breaks name-keyed remapping).
-- MUST be empty (role.name has a UNIQUE constraint, so this is belt+braces).
SELECT 'bap' AS side, name FROM atlas_bap_dashboard.role GROUP BY name HAVING count(*) > 1
UNION ALL
SELECT 'bpp', name FROM atlas_bpp_dashboard.role GROUP BY name HAVING count(*) > 1;

-- 0.5 Target schema must not exist yet.
SELECT nspname FROM pg_namespace WHERE nspname = 'atlas_dashboard';

-- 0.6 BAP persons whose role would be dropped (member of a role that is
-- retired AND has a same-name conflict nowhere). Expected: empty, because
-- retired roles (CUSTOMER, DRIVER) have zero members and INTERNAL_ADMIN is
-- BPP-side (remapped in 03).
SELECT a.id, a.first_name, ra.name
FROM atlas_bap_dashboard.person a
JOIN atlas_bap_dashboard.role ra ON ra.id = a.role_id
WHERE ra.name IN ('CUSTOMER', 'DRIVER', 'INTERNAL_ADMIN');
