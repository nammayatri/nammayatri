-- `is_bpp_sync_needed` marks roles whose holders get a linked entry on the BPP-driver
-- side (e.g. FLEET_OWNER, RENTAL_FLEET_OWNER, DASHBOARD_OPERATOR). Role conversion
-- to/from such roles is blocked dashboard-side (see putAccountUpdateRole / assignRole).
-- Nullable: NULL / false = not sync-needed; true = sync-needed.
-- Seed (which roles get flagged) lives in
-- dev/seed-migrations/provider-dashboard/0002-flag-bpp-sync-roles.sql (this file stays pure DDL).

ALTER TABLE atlas_bpp_dashboard.role
  ADD COLUMN is_bpp_sync_needed boolean;
