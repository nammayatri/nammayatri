CREATE TABLE IF NOT EXISTS atlas_app.merchant_config_new (
    merchant_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_app.merchant (id),
    name character varying(255) NOT NULL,
    origin_restriction text[],
    destination_restriction text[],
    registry_url character varying(255) NOT NULL,
    gateway_url character varying(255) NOT NULL,
    driver_offer_base_url text NOT NULL,
    driver_offer_api_key character varying(128) NOT NULL,
    driver_offer_merchant_id character varying(255) NOT NULL,
    city text NOT NULL DEFAULT 'Kochi'::text,
    geo_hash_precision_value integer NOT NULL DEFAULT 9,
    country text NOT NULL DEFAULT 'India'::text,
    dir_cache_slot json,
    time_diff_from_utc integer NOT NULL DEFAULT 19800,
    distance_weightage integer NOT NULL DEFAULT 60,
    minimum_driver_rates_count integer,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO atlas_app.merchant_config_new
(
    merchant_id,
    name,
    origin_restriction,
    destination_restriction,
    registry_url,
    gateway_url,
    driver_offer_base_url,
    driver_offer_api_key,
    driver_offer_merchant_id,
    city,
    geo_hash_precision_value,
    country,
    dir_cache_slot,
    time_diff_from_utc,
    distance_weightage,
    minimum_driver_rates_count,
    updated_at,
    created_at
)
SELECT
    id,
    name,
    origin_restriction,
    destination_restriction,
    registry_url,
    gateway_url,
    driver_offer_base_url,
    driver_offer_api_key,
    driver_offer_merchant_id,
    city,
    geo_hash_precision_value,
    country,
    dir_cache_slot,
    time_diff_from_utc,
    distance_weightage,
    minimum_driver_rates_count,
    updated_at,
    created_at
FROM atlas_app.merchant;

-- TODO: Needs to be updated in next release

-- ALTER TABLE atlas_app.merchant
--     DROP COLUMN city,
--     DROP COLUMN country,
--     DROP COLUMN origin_restriction,
--     DROP COLUMN destination_restriction,
--     DROP COLUMN driver_offer_base_url,
--     DROP COLUMN driver_offer_api_key,
--     DROP COLUMN driver_offer_merchant_id,
--     DROP COLUMN geo_hash_precision_value,
--     DROP COLUMN minimum_driver_rates_count,
--     DROP COLUMN distance_weightage,
--     DROP COLUMN time_diff_from_utc,
--     DROP COLUMN dir_cache_slot;

ALTER TABLE atlas_app.merchant_config RENAME TO fraud_config;