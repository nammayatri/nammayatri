ALTER TABLE atlas_bap_dashboard.merchant DROP COLUMN email_encrypted;
ALTER TABLE atlas_bap_dashboard.merchant DROP COLUMN email_hash;
ALTER TABLE atlas_bap_dashboard.merchant DROP COLUMN password_hash;
ALTER TABLE atlas_bap_dashboard.merchant DROP COLUMN company_name;
ALTER TABLE atlas_bap_dashboard.person ADD COLUMN dashboard_access_type Text;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN enabled Boolean ;

update atlas_bap_dashboard.merchant set enabled = true; -- query only to fix local , already ran in prod and master.

