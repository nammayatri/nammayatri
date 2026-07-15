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
