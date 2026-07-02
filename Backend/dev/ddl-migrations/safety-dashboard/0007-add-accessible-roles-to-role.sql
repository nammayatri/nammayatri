-- `accessible_roles` drives `/roles/listv2` for non-admin (DASHBOARD_USER) callers —
-- each role lists which role IDs its holders may see. Default '{}' keeps existing
-- rows inert until ops seeds them. DASHBOARD_ADMIN callers bypass this column
-- entirely (they see all roles via listRoles), so no admin seed is needed.
-- Safety-dashboard doesn't expose listv2 today, but the shared Beam binding
-- SELECTs the column so it must exist here too.

ALTER TABLE atlas_safety_dashboard.role
  ADD COLUMN accessible_roles text[] NOT NULL DEFAULT '{}';

CREATE OR REPLACE FUNCTION atlas_safety_dashboard.role_scrub_accessible_roles()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE atlas_safety_dashboard.role
     SET accessible_roles = array_remove(accessible_roles, OLD.id::text)
   WHERE OLD.id::text = ANY(accessible_roles);
  RETURN OLD;
END;
$$;

DROP TRIGGER IF EXISTS role_scrub_accessible_roles ON atlas_safety_dashboard.role;
CREATE TRIGGER role_scrub_accessible_roles
AFTER DELETE ON atlas_safety_dashboard.role
FOR EACH ROW EXECUTE FUNCTION atlas_safety_dashboard.role_scrub_accessible_roles();
