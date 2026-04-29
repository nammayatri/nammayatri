

ALTER TABLE atlas_app.exophone
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_app.merchant_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- TODO : Remove 'merchant_id' columns from the following tables
-- DROP QUERIES (Drop the merchant_id column)
-- ALTER TABLE atlas_app.merchant_service_usage_config
-- DROP COLUMN merchant_id;

-- ALTER TABLE atlas_app.merchant_message
-- DROP COLUMN merchant_id;

-- ALTER TABLE atlas_app.merchant_payment_method
-- DROP COLUMN merchant_id;

-- ALTER TABLE atlas_app.exophone
-- DROP COLUMN merchant_id;

-- ALTER TABLE atlas_app.merchant_config
-- DROP COLUMN merchant_id;
----------------------------------------------------------------------- END --------------------------------------------------------------------------

-- Add merchant_operating_city_id column to required tables

------------------------------------------------------------- Geometry Table Migrations --------------------------------------------------------------
-- Add new column 'city'
ALTER TABLE atlas_app.geometry
ADD COLUMN city character varying(255) NULL;

-- Set 'city' column as NOT NULL
ALTER TABLE atlas_app.geometry
ALTER COLUMN city SET NOT NULL;
----------------------------------------------------------------------- END --------------------------------------------------------------------------