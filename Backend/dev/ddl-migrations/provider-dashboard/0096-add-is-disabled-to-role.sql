-- `is_disabled` soft-disables a role. A disabled role has had its holders migrated
-- to a replacement role (same dashboard_access_type) and its access_matrix rows
-- removed; the row is retained rather than hard-deleted. See disableRole in
-- Domain.Action.Dashboard.Roles.
-- Nullable: NULL / false = active; true = disabled.

-- IF NOT EXISTS: this file was previously numbered 0091. postgresql-simple-migration keys
-- schema_migrations on filename, so any environment that already applied it under the old name
-- will re-run it under the new one; the guard makes that re-run inert instead of a hard failure.
ALTER TABLE atlas_dashboard.role
  ADD COLUMN IF NOT EXISTS is_disabled boolean;
