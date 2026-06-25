-- `accessible_roles` drives `/roles/listv2` — each role lists which role IDs
-- its holders are allowed to see/assign. Default '{}' keeps existing roles
-- inert until ops manually seeds them.

ALTER TABLE atlas_bap_dashboard.role
  ADD COLUMN accessible_roles text[] NOT NULL DEFAULT '{}';

-- Seed every DASHBOARD_ADMIN-tier role with the full set of role IDs so admins
-- aren't locked out of role-picker UIs the moment the new endpoint ships.
-- New roles created after this migration must be appended manually (or via a
-- handler-side update in `createRole`); the seed is one-shot.
UPDATE atlas_bap_dashboard.role
   SET accessible_roles = (SELECT array_agg(id) FROM atlas_bap_dashboard.role)
 WHERE dashboard_access_type = 'DASHBOARD_ADMIN';

-- When a role is deleted, scrub its id from every other row's accessible_roles.
-- Without this, stale ids accumulate forever (functionally harmless because
-- findAllByIds drops missing rows, but it grows the array unbounded).
CREATE OR REPLACE FUNCTION atlas_bap_dashboard.role_scrub_accessible_roles()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE atlas_bap_dashboard.role
     SET accessible_roles = array_remove(accessible_roles, OLD.id::text);
  RETURN OLD;
END;
$$;

DROP TRIGGER IF EXISTS role_scrub_accessible_roles ON atlas_bap_dashboard.role;
CREATE TRIGGER role_scrub_accessible_roles
AFTER DELETE ON atlas_bap_dashboard.role
FOR EACH ROW EXECUTE FUNCTION atlas_bap_dashboard.role_scrub_accessible_roles();
