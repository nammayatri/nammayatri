

-- ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
-- ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city (id);

-- -- SETTING COLUMN AS NOT NULL
ALTER TABLE atlas_driver_offer_bpp.go_home_config
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
-- ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- ALTER TABLE atlas_driver_offer_bpp.driver_intelligent_pool_config
-- ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant_message
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant_overlay
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.exophone
ALTER COLUMN merchant_operating_city_id SET NOT NULL;

-- DROPPING PRIMARY KEY CONSTRAINTS
ALTER TABLE atlas_driver_offer_bpp.go_home_config
DROP CONSTRAINT go_home_config_pkey;

ALTER TABLE atlas_driver_offer_bpp.merchant_message
DROP CONSTRAINT merchant_message_pkey;

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
DROP CONSTRAINT PK_onboarding_document_configs;

-- ADDING MERCHANT_OPERATING_CITY_ID AS PRIMARY KEY
ALTER TABLE atlas_driver_offer_bpp.go_home_config
ADD PRIMARY KEY (merchant_operating_city_id);

ALTER TABLE atlas_driver_offer_bpp.merchant_message
ADD PRIMARY KEY (merchant_operating_city_id, message_key);

ALTER TABLE atlas_driver_offer_bpp.onboarding_document_configs
ADD PRIMARY KEY (merchant_operating_city_id, document_type);

--ride
ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_merchant_id FOREIGN KEY (merchant_id) REFERENCES atlas_driver_offer_bpp.merchant(id);
ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_merchant_operating_city_id FOREIGN KEY (merchant_operating_city_id) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);