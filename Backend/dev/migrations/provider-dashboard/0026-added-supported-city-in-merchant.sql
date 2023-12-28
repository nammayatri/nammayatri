ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN default_operating_city Text;
ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN supported_operating_cities Text[];
ALTER TABLE atlas_bpp_dashboard.merchant_access ADD COLUMN merchant_short_id Text;

---Update the default operating city for the existing merchants ,
UPDATE atlas_bpp_dashboard.merchant
SET default_operating_city = 'Bangalore', supported_operating_cities = ARRAY['Bangalore']
WHERE short_id = 'NAMMA_YATRI_PARTNER';

UPDATE atlas_bpp_dashboard.merchant
SET default_operating_city = 'Kochi', supported_operating_cities = ARRAY['Kochi']
WHERE short_id = 'YATRI_PARTNER';

UPDATE atlas_bpp_dashboard.merchant
SET default_operating_city = 'Kolkata', supported_operating_cities = ARRAY ['Kolkata']
WHERE short_id = 'JATRI_SAATHI_PARTNER';

UPDATE atlas_bpp_dashboard.merchant
SET default_operating_city = 'Paris', supported_operating_cities = ARRAY ['Paris']
WHERE short_id = 'PASSCULTURE_PARTNER';

-- DON'T RUN THIS QUERY IN MASTER / PROD - For Local builds only.
UPDATE atlas_bpp_dashboard.merchant
SET default_operating_city = 'Kochi', supported_operating_cities = ARRAY['Bangalore', 'Kolkata', 'Kochi']
WHERE short_id IN ('YATRI_PARTNER', 'NAMMA_YATRI_PARTNER', 'SPECIAL_ZONE', 'JATRI_SAATHI_PARTNER');

-- update the merchant short id for the existing merchants in merchant_access table
UPDATE atlas_bpp_dashboard.merchant_access AS ma
SET merchant_short_id = m.short_id
FROM atlas_bpp_dashboard.merchant AS m
WHERE ma.merchant_id = m.id;

-- Set all the new added fields NOT NULL.
ALTER TABLE atlas_bpp_dashboard.merchant
ALTER COLUMN default_operating_city SET NOT NULL;

ALTER TABLE atlas_bpp_dashboard.merchant
ALTER COLUMN supported_operating_cities SET NOT NULL;

ALTER TABLE atlas_bpp_dashboard.merchant_access
ALTER COLUMN merchant_short_id SET NOT NULL;
