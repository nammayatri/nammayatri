-- `is_disabled` soft-disables a role. A disabled role has had its holders migrated
-- to a replacement role (same dashboard_access_type) and its access_matrix rows
-- removed; the row is retained rather than hard-deleted. See disableRole in
-- Domain.Action.Dashboard.Roles.
-- Nullable: NULL / false = active; true = disabled.

ALTER TABLE atlas_bap_dashboard.role
  ADD COLUMN is_disabled boolean;
