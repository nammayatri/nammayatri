CREATE TABLE atlas_app.merchant_payment_method ();

ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN collected_by character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN payment_instrument character varying(255) NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN payment_type character varying(30) NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN priority int NOT NULL;
ALTER TABLE atlas_app.merchant_payment_method ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_payment_method ADD PRIMARY KEY ( id);