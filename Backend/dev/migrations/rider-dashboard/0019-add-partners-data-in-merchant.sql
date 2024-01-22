ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN company_name Text;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN domain Text;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN website Text;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN email_hash Text;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN password_hash Text;
ALTER TABLE atlas_bap_dashboard.merchant ADD COLUMN email_encrypted Text;
ALTER TABLE atlas_bap_dashboard.merchant ALTER COLUMN server_name DROP NOT NULL;