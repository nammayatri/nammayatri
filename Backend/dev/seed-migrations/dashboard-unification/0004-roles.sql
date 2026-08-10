-- ============================================================================
-- Phase 1 / 0004: merge roles. BPP ids are canonical for shared names (person
-- and access_matrix rows on the BPP side then copy without remapping).
-- Skipped: INTERNAL_ADMIN (retired -> members become JUSPAY_ADMIN in 03),
-- CUSTOMER / DRIVER (retired app-user relics), and any role with zero
-- members on BOTH sides — except FINANCE_ADMIN and SlaMonitoring, which are
-- forward-looking bundles referenced by the control-center matrix.
-- ============================================================================

CREATE TEMP VIEW role_members AS
SELECT r.name,
       coalesce(b.cnt, 0) AS bap_members,
       coalesce(p.cnt, 0) AS bpp_members
FROM (SELECT name FROM atlas_dashboard.bap_role
      UNION SELECT name FROM atlas_dashboard.bpp_role) r(name)
LEFT JOIN (SELECT ro.name, count(*) cnt FROM atlas_dashboard.bap_person pe
           JOIN atlas_dashboard.bap_role ro ON ro.id = pe.role_id GROUP BY ro.name) b ON b.name = r.name
LEFT JOIN (SELECT ro.name, count(*) cnt FROM atlas_dashboard.bpp_person pe
           JOIN atlas_dashboard.bpp_role ro ON ro.id = pe.role_id GROUP BY ro.name) p ON p.name = r.name;

CREATE TEMP VIEW roles_to_keep AS
SELECT name FROM role_members
WHERE (bap_members + bpp_members > 0 OR name IN ('FINANCE_ADMIN', 'SlaMonitoring', 'MTC_OPS_READ', 'MTC_OPS', 'CHENNAI_CONDUCTOR', 'STUDENT_PASS_DEPOT','PT_DEPOT_MANAGER','PT_CONDUCTOR','MtcFleetOps','MTC_OPS_READ','MTC_ADMIN','MTC WAYBILL','MTC OPS','MTC','CUMTA','CMRL','CLG_ADMIN','CHENNAI_CONDUCTOR','ANALYTICS'))
  AND name NOT IN ('INTERNAL_ADMIN', 'CUSTOMER', 'DRIVER');

-- 2.1 BPP roles (canonical ids).
INSERT INTO atlas_dashboard.role
  (id, name, dashboard_access_type, description, accessible_roles, is_bpp_sync_needed, created_at, updated_at)
SELECT r.id, r.name, r.dashboard_access_type, r.description, r.accessible_roles,
       r.is_bpp_sync_needed, r.created_at, r.updated_at
FROM atlas_dashboard.bpp_role r
WHERE r.name IN (SELECT name FROM roles_to_keep);

-- 2.2 BAP-only roles (keep their BAP ids; no collision possible — UUIDs,
-- and 0.2/0.4 preflights passed).
INSERT INTO atlas_dashboard.role
  (id, name, dashboard_access_type, description, accessible_roles, is_bpp_sync_needed, created_at, updated_at)
SELECT r.id, r.name, r.dashboard_access_type, r.description, r.accessible_roles,
       r.is_bpp_sync_needed, r.created_at, r.updated_at
FROM atlas_dashboard.bap_role r
WHERE r.name IN (SELECT name FROM roles_to_keep)
  AND r.name NOT IN (SELECT name FROM atlas_dashboard.bpp_role);

-- 2.3 Scrub accessible_roles arrays of ids that did not survive the merge
-- (dropped roles, or BAP ids of shared-name roles that now use the BPP id).
UPDATE atlas_dashboard.role r
SET accessible_roles = coalesce(
  (SELECT array_agg(x) FROM unnest(r.accessible_roles) x
   WHERE x IN (SELECT id FROM atlas_dashboard.role)), '{}');

-- 2.4 Sanity: every kept name present exactly once.
SELECT count(*) AS merged_roles,
       (SELECT count(*) FROM roles_to_keep) AS expected
FROM atlas_dashboard.role;
