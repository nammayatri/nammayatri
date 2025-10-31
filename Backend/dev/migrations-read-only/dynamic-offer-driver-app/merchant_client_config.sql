CREATE TABLE atlas_driver_offer_bpp.merchant_client_config ();

ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN config_json json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN service_name character varying(50) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN package_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD PRIMARY KEY ( service_name, package_id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ALTER COLUMN package_id TYPE text;
ALTER TABLE atlas_driver_offer_bpp.merchant_client_config ADD COLUMN client_os text ;