CREATE TABLE atlas_app.payment_transaction_offer_payout ();

ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN amount_paid_by_user double precision NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN booking_created_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN network_order_id text NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN offer_code text NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN offer_id text NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN order_amount double precision NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN payment_rrn text NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN payment_txn_id text NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN provider_agency text NOT NULL;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN settlement_reference text ;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN settlement_ts timestamp with time zone ;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_transaction_offer_payout ADD PRIMARY KEY ( id);
