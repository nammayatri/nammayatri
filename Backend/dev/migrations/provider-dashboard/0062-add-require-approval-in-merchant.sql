ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN require_admin_approval_for_fleet_onboarding boolean DEFAULT false;

UPDATE atlas_bpp_dashboard.merchant
SET require_admin_approval_for_fleet_onboarding = true
WHERE short_id = 'MSIL_PARTNER';