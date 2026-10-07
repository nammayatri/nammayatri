CREATE TABLE atlas_driver_offer_bpp.payment_customer ();

ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN client_auth_token text ;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN client_auth_token_expiry timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN customer_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN payment_service_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN service_name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payment_customer ADD PRIMARY KEY ( driver_id, service_name);
