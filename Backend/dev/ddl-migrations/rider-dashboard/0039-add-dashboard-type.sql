-- Add dashboard_type column with default value 'DEFAULT_DASHBOARD'
ALTER TABLE atlas_bap_dashboard.person
ADD COLUMN dashboard_type text NOT NULL DEFAULT 'DEFAULT_DASHBOARD';

ALTER TABLE atlas_bap_dashboard.person
DROP CONSTRAINT IF EXISTS unique_email,
DROP CONSTRAINT IF EXISTS unique_mobile_number_country_code;

-- Add new constraints that include dashboard_type
ALTER TABLE atlas_bap_dashboard.person
ADD CONSTRAINT unique_email_dashboard_type UNIQUE (email_hash, dashboard_type),
ADD CONSTRAINT unique_mobile_number_country_code_dashboard_type UNIQUE (mobile_country_code, mobile_number_hash, dashboard_type);