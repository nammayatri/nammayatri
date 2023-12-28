ALTER TABLE atlas_driver_offer_bpp.geometry
ADD COLUMN city character varying(255) NULL;

-- Update the values of the 'city' column
-- NOTE: When running in master, need to first fix master merchant table for it.
UPDATE atlas_driver_offer_bpp.geometry
SET city = merchant.city
FROM atlas_driver_offer_bpp.merchant
WHERE (atlas_driver_offer_bpp.geometry.region = ANY (merchant.origin_restriction) OR
       atlas_driver_offer_bpp.geometry.region = ANY (merchant.destination_restriction)
      );

ALTER TABLE atlas_driver_offer_bpp.geometry
ALTER COLUMN city SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_special_zone
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

-- Changing primary-key for driver_pool_config
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config
ADD COLUMN id character(36);

UPDATE atlas_driver_offer_bpp.driver_pool_config
SET id = md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID;

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config
DROP CONSTRAINT driver_pool_config_pkey;

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config
ADD PRIMARY KEY (id);

-- Dropping indexes based on merchant_id
DROP INDEX atlas_driver_offer_bpp.idx_fare_product;
DROP INDEX atlas_driver_offer_bpp.idx_merchant_overlay_key;

-- Creating new indexes based on merchant_operating_city_id
CREATE INDEX idx_fare_product ON atlas_driver_offer_bpp.fare_product USING btree (merchant_operating_city_id, vehicle_variant, "area");
CREATE INDEX idx_merchant_overlay_key ON atlas_driver_offer_bpp.merchant_overlay USING btree (merchant_operating_city_id, overlay_key);