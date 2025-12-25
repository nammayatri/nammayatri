CREATE TABLE atlas_app.merchant_service_config ();

ALTER TABLE atlas_app.merchant_service_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_service_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_service_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_service_config ADD COLUMN config_json json NOT NULL;
ALTER TABLE atlas_app.merchant_service_config ADD COLUMN service_name character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_service_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_service_config ADD PRIMARY KEY ( merchant_id, service_name);


------- SQL updates -------

ALTER TABLE atlas_app.merchant_service_config ALTER COLUMN service_name TYPE character varying(50);