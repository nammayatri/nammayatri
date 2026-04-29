-- Add new column 'operating_city' in merchant_access table
ALTER TABLE atlas_bap_dashboard.merchant_access
ADD COLUMN operating_city VARCHAR(255) NULL;

-- Set 'operating_city' column as NOT NULL
ALTER TABLE atlas_bap_dashboard.merchant_access
ALTER COLUMN operating_city SET NOT NULL;

-- Add new unique key of 'person_id', 'merchant_id' and 'operating_city'
ALTER TABLE atlas_bap_dashboard.merchant_access
ADD CONSTRAINT unique_person_id_merchant_id_operating_city UNIQUE (person_id, merchant_id, operating_city);

----------------------------------------- REGISTRATION TOKEN TABLE MIGRATIONS -----------------------------------------
-- Add new column 'operating_city' in 'registration_token' table
ALTER TABLE atlas_bap_dashboard.registration_token
ADD COLUMN operating_city VARCHAR(255) NULL;

-- Set 'operating_city' column as NOT NULL
ALTER TABLE atlas_bap_dashboard.registration_token
ALTER COLUMN operating_city SET NOT NULL;
--------------------------------------------------------- END ---------------------------------------------------------
