ALTER TABLE atlas_driver_offer_bpp.transporter_config RENAME TO merchant_config;


ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN gstin character varying(255);
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN name character varying(255);
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN verified boolean;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN enabled boolean;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN description text;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN mobile_number text;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN mobile_country_code character varying(255);
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN from_time timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN to_time timestamp with time zone;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN api_key text;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN head_count bigint;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN status character varying(255);
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN info text;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN origin_restriction text[];
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN destination_restriction text[];
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN internal_api_key character varying(128);
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN city text;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN geo_hash_precision_value integer;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN country text;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN minimum_driver_rates_count int;
ALTER TABLE atlas_driver_offer_bpp.merchant_config ADD COLUMN registry_url character varying(255) NOT NULL DEFAULT 'http://localhost:8020';

UPDATE atlas_driver_offer_bpp.merchant_config
    SET gstin = (SELECT gstin FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        name = (SELECT name FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        verified = (SELECT verified FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        enabled = (SELECT enabled FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        description = (SELECT description FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        mobile_number = (SELECT mobile_number FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        mobile_country_code = (SELECT mobile_country_code FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        from_time = (SELECT from_time FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        to_time = (SELECT to_time FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        api_key = (SELECT api_key FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        head_count = (SELECT head_count FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        status = (SELECT status FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        info = (SELECT info FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        origin_restriction = (SELECT origin_restriction FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        destination_restriction = (SELECT destination_restriction FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        internal_api_key = (SELECT internal_api_key FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        city = (SELECT city FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        geo_hash_precision_value = (SELECT geo_hash_precision_value FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        country = (SELECT country FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        minimum_driver_rates_count = (SELECT minimum_driver_rates_count FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id),
        registry_url = (SELECT registry_url FROM atlas_driver_offer_bpp.merchant WHERE merchant.id = merchant_config.merchant_id);


-- TODO: Needs to be updated in next release

-- ALTER TABLE atlas_driver_offer_bpp.merchant
--     DROP COLUMN gstin,
--     DROP COLUMN verified,
--     DROP COLUMN enabled,
--     DROP COLUMN mobile_number,
--     DROP COLUMN mobile_country_code,
--     DROP COLUMN from_time,
--     DROP COLUMN to_time,
--     DROP COLUMN api_key,
--     DROP COLUMN head_count,
--     DROP COLUMN info,
--     DROP COLUMN registry_url,
--     DROP COLUMN origin_restriction,
--     DROP COLUMN destination_restriction,
--     DROP COLUMN internal_api_key,
--     DROP COLUMN city,
--     DROP COLUMN geo_hash_precision_value,
--     DROP COLUMN country,
--     DROP COLUMN minimum_driver_rates_count;