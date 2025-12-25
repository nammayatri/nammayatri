UPDATE atlas_bpp_dashboard.role set dashboard_access_type = 'FLEET_OWNER' where name = 'FLEET';

UPDATE atlas_bpp_dashboard.merchant
SET require_admin_approval_for_fleet_onboarding = false
WHERE short_id = 'MSIL_PARTNER';