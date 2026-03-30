------------------------------------------------------------------------------------------------
-- NOTE: This is to fix local schema to make it similar as master schema for config-sync import.
-- Do not run in master or prod
------------------------------------------------------------------------------------------------

-- Multiple rows per place_id are valid (different service_name + city per place)
ALTER TABLE atlas_app.place_based_service_config DROP CONSTRAINT IF EXISTS place_based_service_config_pkey;
ALTER TABLE atlas_app.place_based_service_config ADD PRIMARY KEY (place_id, service_name, merchant_operating_city_id);
