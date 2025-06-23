ALTER TABLE atlas_bpp_dashboard.merchant DROP COLUMN email_encrypted;
ALTER TABLE atlas_bpp_dashboard.merchant DROP COLUMN email_hash;
ALTER TABLE atlas_bpp_dashboard.merchant DROP COLUMN password_hash;
ALTER TABLE atlas_bpp_dashboard.merchant DROP COLUMN company_name;
ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN auth_token Text;
ALTER TABLE atlas_bpp_dashboard.person ADD COLUMN dashboard_access_type Text;
ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN enabled Boolean;
update atlas_bpp_dashboard.merchant set enabled = true;  -- query only to fix local , already ran in prod and master.

-- ONLY FOR LOCAL
UPDATE atlas_bpp_dashboard.person SET dashboard_access_type = 'DASHBOARD_OPERATOR'
    WHERE id = 'favorit-operator-0-0000-000000000000';
UPDATE atlas_bpp_dashboard.person SET dashboard_access_type = 'FLEET_OWNER'
    WHERE id = 'favorit-fleet-owner-0000000000000000';
UPDATE atlas_bpp_dashboard.person SET dashboard_access_type = 'RENTAL_FLEET_OWNER'
    WHERE id = 'favorit-rental_fleet-owner-000000000';