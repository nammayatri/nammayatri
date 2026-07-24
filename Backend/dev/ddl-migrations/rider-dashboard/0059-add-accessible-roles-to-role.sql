-- `accessible_roles` drives `/roles/listv2` for non-admin (DASHBOARD_USER) callers —
-- each role lists which role IDs its holders may see. Default '{}' keeps existing
-- rows inert until ops seeds them. DASHBOARD_ADMIN callers bypass this column
-- entirely (they see all roles via listRoles), so no admin seed is needed.

ALTER TABLE atlas_bap_dashboard.role
  ADD COLUMN accessible_roles text[] NOT NULL DEFAULT '{}';
