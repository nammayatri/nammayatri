ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN default_operating_city Text;
ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN supported_operating_cities Text[];
ALTER TABLE atlas_bpp_dashboard.merchant_access ADD COLUMN merchant_short_id Text;

-- Set all the new added fields NOT NULL.
ALTER TABLE atlas_bpp_dashboard.merchant
ALTER COLUMN default_operating_city SET NOT NULL;

ALTER TABLE atlas_bpp_dashboard.merchant
ALTER COLUMN supported_operating_cities SET NOT NULL;

ALTER TABLE atlas_bpp_dashboard.merchant_access
ALTER COLUMN merchant_short_id SET NOT NULL;
