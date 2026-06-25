-- `accessible_roles` drives `/roles/listv2` — each role lists which role IDs
-- its holders are allowed to see/assign. Safety-dashboard doesn't expose
-- listv2 today, but the shared Beam binding SELECTs the column so it must
-- exist here too.

ALTER TABLE atlas_safety_dashboard.role
  ADD COLUMN accessible_roles text[] NOT NULL DEFAULT '{}';

-- Seed every DASHBOARD_ADMIN-tier role with the full set of role IDs so admins
-- aren't locked out of role-picker UIs the moment the new endpoint ships.
-- New roles created after this migration must be appended manually (or via a
-- handler-side update in `createRole`); the seed is one-shot.
UPDATE atlas_safety_dashboard.role
   SET accessible_roles = (SELECT array_agg(id) FROM atlas_safety_dashboard.role)
 WHERE dashboard_access_type = 'DASHBOARD_ADMIN';

CREATE OR REPLACE FUNCTION atlas_safety_dashboard.role_scrub_accessible_roles()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE atlas_safety_dashboard.role
     SET accessible_roles = array_remove(accessible_roles, OLD.id::text);
  RETURN OLD;
END;
$$;

DROP TRIGGER IF EXISTS role_scrub_accessible_roles ON atlas_safety_dashboard.role;
CREATE TRIGGER role_scrub_accessible_roles
AFTER DELETE ON atlas_safety_dashboard.role
FOR EACH ROW EXECUTE FUNCTION atlas_safety_dashboard.role_scrub_accessible_roles();
