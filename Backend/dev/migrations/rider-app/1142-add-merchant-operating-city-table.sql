CREATE TABLE atlas_app.merchant_operating_city (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id),
    city character varying(255) NOT NULL
);

INSERT INTO atlas_app.merchant_operating_city VALUES ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a50','da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'BANGALORE');
INSERT INTO atlas_app.merchant_operating_city VALUES ('da3e23a5-3ce6-4c37-8b9b-41377c3c1a50','da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'KOCHI');

-- Rename the table
ALTER TABLE atlas_app.merchant_operating_city RENAME TO _merchant_operating_city_t;

-- Add the new column
ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app._merchant_operating_city_t(id);
ALTER TABLE atlas_app.merchant_service_config ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_app._merchant_operating_city_t(id);
-- Update the values of the new column
UPDATE atlas_app.merchant_service_usage_config
SET merchant_operating_city_id = _merchant_operating_city_t.id
FROM atlas_app._merchant_operating_city_t
WHERE atlas_app.merchant_service_usage_config.merchant_id = _merchant_operating_city_t.merchant_id;

UPDATE atlas_app.merchant_service_config
SET merchant_operating_city_id = _merchant_operating_city_t.id
FROM atlas_app._merchant_operating_city_t
WHERE atlas_app.merchant_service_config.merchant_id = _merchant_operating_city_t.merchant_id;

-- Set the column as NOT NULL
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN merchant_operating_city_id SET NOT NULL;
ALTER TABLE atlas_app.merchant_service_config ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- Drop the primary key constraint
ALTER TABLE atlas_app.merchant_service_usage_config DROP CONSTRAINT merchant_service_usage_config_pkey;
ALTER TABLE atlas_app.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;

-- Add the merchant_operating_city_id column as the primary key
ALTER TABLE atlas_app.merchant_service_usage_config ADD PRIMARY KEY (merchant_operating_city_id);
ALTER TABLE atlas_app.merchant_service_config ADD PRIMARY KEY (merchant_operating_city_id, service_name);
-- Drop the merchant_id column

ALTER TABLE atlas_app.registration_token ADD COLUMN merchant_operating_city_id TEXT NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a50';

ALTER TABLE atlas_app.search_request ADD COLUMN merchant_operating_city_id character(36) NOT NULL REFERENCES atlas_app._merchant_operating_city_t(id);

ALTER TABLE atlas_app.booking ADD COLUMN merchant_operating_city_id character(36) NOT NULL REFERENCES atlas_app._merchant_operating_city_t(id);

ALTER TABLE atlas_app.estimate ADD COLUMN merchant_operating_city_id character(36)  REFERENCES atlas_app._merchant_operating_city_t(id) ;
ALTER TABLE atlas_app.driver_offer ADD COLUMN merchant_operating_city_id character(36)  REFERENCES atlas_app._merchant_operating_city_t(id) ;
ALTER TABLE atlas_app.quote ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL REFERENCES atlas_app._merchant_operating_city_t (id);
ALTER TABLE atlas_app.ride ADD COLUMN merchant_operating_city_id character(36)  REFERENCES atlas_app._merchant_operating_city_t(id) ;

-- Drop queries (Drop the merchant_id column)
ALTER TABLE atlas_app.merchant_service_usage_config DROP COLUMN merchant_id;
ALTER TABLE atlas_app.merchant_service_config DROP COLUMN merchant_id;
