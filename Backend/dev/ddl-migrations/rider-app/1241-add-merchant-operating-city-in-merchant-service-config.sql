

ALTER TABLE atlas_app.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;
ALTER TABLE atlas_app.merchant_service_config ADD PRIMARY KEY(service_name, merchant_operating_city_id);