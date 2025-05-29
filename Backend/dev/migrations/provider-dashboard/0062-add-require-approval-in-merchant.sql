ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN require_admin_approval_for_fleet_onboarding boolean DEFAULT false;
ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN has_fleet_member_hierarchy boolean DEFAULT true;


UPDATE atlas_bpp_dashboard.merchant
SET require_admin_approval_for_fleet_onboarding = true,
    has_fleet_member_hierarchy = false
WHERE short_id = 'MSIL_PARTNER';

ALTER table atlas_bpp_dashboard.merchant ADD COLUMN verify_fleet_while_login boolean DEFAULT true;

UPDATE atlas_bpp_dashboard.merchant
SET verify_fleet_while_login = false
WHERE short_id = 'MSIL_PARTNER';
