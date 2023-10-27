CREATE TABLE atlas_app.merchant_operating_city (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    merchant_short_id character varying(255) NOT NULL REFERENCES atlas_app.merchant (short_id),
    city character varying(255) NOT NULL
);

-- Inserting values in merchant_operating_city table
INSERT INTO atlas_app.merchant_operating_city (id, merchant_id, merchant_short_id, city)
SELECT
    atlas_app.uuid_generate_v4() AS id,
    id AS merchant_id,
    short_id AS merchant_short_id,
    city
FROM atlas_app.merchant;

----------------------------------------------------- Service Usage Config / Message / PaymentMethod / Exophone / Config Table Migrations -----------------------------------------------
-- Add the new column
ALTER TABLE atlas_app.merchant_service_usage_config
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.merchant_message
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.merchant_payment_method
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.exophone
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.merchant_config
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

-- Update the values of the new column
UPDATE atlas_app.merchant_service_usage_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_app.merchant_operating_city
WHERE atlas_app.merchant_service_usage_config.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_app.merchant_message
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_app.merchant_operating_city
WHERE atlas_app.merchant_message.merchant_id = merchant_operating_city.merchant_id;

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

-- Set the column as NOT NULL
ALTER TABLE atlas_app.merchant_service_usage_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_app.merchant_message
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_app.merchant_payment_method
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_app.exophone
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_app.merchant_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- Drop the primary key constraint
ALTER TABLE atlas_app.merchant_service_usage_config
DROP CONSTRAINT merchant_service_usage_config_pkey;

ALTER TABLE atlas_app.merchant_message
DROP CONSTRAINT merchant_message_pkey;

-- Add the merchant_operating_city_id column as the primary key
ALTER TABLE atlas_app.merchant_service_usage_config
ADD PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_app.merchant_message
ADD PRIMARY KEY (merchant_operating_city_id, message_key);

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
ALTER TABLE atlas_app.search_request
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.estimate
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.quote
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.driver_offer
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.booking
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

ALTER TABLE atlas_app.ride
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id);

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
ALTER TABLE atlas_app.person
ADD COLUMN current_city               character varying(255) NULL,
-- Add new column 'merchant_operating_city_id'
ADD COLUMN merchant_operating_city_id character(36)          NULL;

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