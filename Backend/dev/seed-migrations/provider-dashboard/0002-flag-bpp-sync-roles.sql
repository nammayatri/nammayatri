-- Flag roles whose holders need a linked entry on the BPP-driver side
-- (fleet-owner / operator access types). Schema-coupled seed for the
-- atlas_bpp_dashboard.role.is_bpp_sync_needed column added in
-- dev/ddl-migrations/provider-dashboard/0090-add-is-bpp-sync-needed-to-role.sql.
-- Role conversion to/from these roles is blocked dashboard-side, and creating a
-- person with such a role also provisions a driver-side entry.
UPDATE atlas_bpp_dashboard.role
  SET is_bpp_sync_needed = true
  WHERE dashboard_access_type IN ('FLEET_OWNER', 'RENTAL_FLEET_OWNER', 'DASHBOARD_OPERATOR');
