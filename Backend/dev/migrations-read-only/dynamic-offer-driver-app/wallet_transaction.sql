CREATE TABLE atlas_driver_offer_bpp.wallet_transaction ();

ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN payment_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.wallet_transaction ADD PRIMARY KEY ( id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY wallet_transaction_idx_driver_id ON atlas_driver_offer_bpp.wallet_transaction USING btree (driver_id);
CREATE INDEX CONCURRENTLY wallet_transaction_idx_payment_order_id ON atlas_driver_offer_bpp.wallet_transaction USING btree (payment_order_id);