-- `accessible_roles` drives `/roles/listv2` for non-admin (DASHBOARD_USER) callers —
-- each role lists which role IDs its holders may see. Default '{}' keeps existing
-- rows inert until ops seeds them. DASHBOARD_ADMIN callers bypass this column
-- entirely (they see all roles via listRoles), so no admin seed is needed.

ALTER TABLE atlas_bap_dashboard.role
  ADD COLUMN accessible_roles text[] NOT NULL DEFAULT '{}';

-- When a role is deleted, scrub its id from every other row's accessible_roles.
-- Without this, stale ids accumulate forever (functionally harmless because
-- findAllByIds drops missing rows, but it grows the array unbounded).
CREATE OR REPLACE FUNCTION atlas_bap_dashboard.role_scrub_accessible_roles()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE atlas_bap_dashboard.role
     SET accessible_roles = array_remove(accessible_roles, OLD.id::text)
   WHERE OLD.id::text = ANY(accessible_roles);
  RETURN OLD;
END;
$$;

DROP TRIGGER IF EXISTS role_scrub_accessible_roles ON atlas_bap_dashboard.role;
CREATE TRIGGER role_scrub_accessible_roles
AFTER DELETE ON atlas_bap_dashboard.role
FOR EACH ROW EXECUTE FUNCTION atlas_bap_dashboard.role_scrub_accessible_roles();
