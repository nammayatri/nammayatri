-- QUERIE ONLY FOR LOCAL (DO NOT RUN IN PROD OR MASTER)
UPDATE atlas_bap_dashboard.merchant
SET supported_operating_cities = ARRAY['Bangalore', 'Kochi']
WHERE id = '94bbea0d-3c52-479b-81f5-eca4969ae797';