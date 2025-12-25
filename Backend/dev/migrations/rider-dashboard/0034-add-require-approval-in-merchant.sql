ALTER table atlas_bap_dashboard.merchant ADD COLUMN require_admin_approval_for_fleet_onboarding Boolean DEFAULT false;

ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN has_fleet_member_hierarchy boolean DEFAULT false;

ALTER table atlas_bap_dashboard.merchant ADD COLUMN verify_fleet_while_login boolean DEFAULT true;
