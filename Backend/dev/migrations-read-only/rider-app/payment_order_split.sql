CREATE TABLE atlas_app.payment_order_split ();

ALTER TABLE atlas_app.payment_order_split ADD COLUMN currency text ;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN mdr_borne_by text NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN merchant_commission double precision NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN payment_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_order_split ADD COLUMN vendor_id text NOT NULL;
ALTER TABLE atlas_app.payment_order_split ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.payment_order_split ADD COLUMN transaction_id text ;