-- INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city)
-- SELECT
--     atlas_app.uuid_generate_v4() AS id,
--     id AS merchant_id,
--     short_id AS merchant_short_id,
--     city
-- FROM atlas_app.merchant;

-- -- for local testing only
-- INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city) VALUES
-- ('namma-yatri-0-0000-0000-00000000city', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'NAMMA_YATRI', 'Kochi'),
-- ('yatri-00-0000-0000-0000-00000000city', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'YATRI', 'Kochi');

----------------------------------------------------- Service Usage Config / Message / PaymentMethod / Exophone / Config Table Migrations -----------------------------------------------
-- Update the values of the new column
UPDATE atlas_app.merchant_payment_method
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_app.merchant_operating_city
WHERE atlas_app.merchant_payment_method.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_app.exophone
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_app.merchant_operating_city
WHERE atlas_app.exophone.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_app.merchant_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_app.merchant_operating_city
WHERE atlas_app.merchant_config.merchant_id = merchant_operating_city.merchant_id;

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

-- Update the values of the 'city' column
-- NOTE: When running in master, need to first fix master merchant table for it.
UPDATE atlas_app.geometry
SET city = merchant.city
FROM atlas_app.merchant
WHERE (atlas_app.geometry.region = ANY (merchant.origin_restriction) OR
       atlas_app.geometry.region = ANY (merchant.destination_restriction)
      );

-- Set 'city' column as NOT NULL
ALTER TABLE atlas_app.geometry
ALTER COLUMN city SET NOT NULL;
----------------------------------------------------------------------- END --------------------------------------------------------------------------

------------------------------------------------------------- Person Table Migrations ----------------------------------------------------------------
-- Add new column 'current_city'

-- Update the values of the 'current_city' column
UPDATE atlas_app.person
SET current_city = merchant.city
FROM atlas_app.merchant
WHERE atlas_app.person.merchant_id = merchant.id;

-- Update the values of the 'merchant_operating_city_id' column
UPDATE atlas_app.person
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_app.merchant_operating_city
WHERE (atlas_app.person.merchant_id = merchant_operating_city.merchant_id
        AND atlas_app.person.current_city = merchant_operating_city.city);
----------------------------------------------------------------------- END --------------------------------------------------------------------------