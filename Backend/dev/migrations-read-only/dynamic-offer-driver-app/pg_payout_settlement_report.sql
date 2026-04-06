CREATE TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ();

ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN bank_name text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN beneficiary_account_number text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN beneficiary_ifsc text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN beneficiary_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_instrument_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_method text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_response_code text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_response_message text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN fulfillment_status text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN order_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN payment_gateway text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN payout_customer_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN payout_request_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN raw_data text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN recon_message text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN recon_status text NOT NULL default 'PENDING';
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN reference_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN rrn text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN settlement_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN settlement_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN settlement_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN settlement_mode text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN settlement_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN txn_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN txn_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN txn_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN txn_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD COLUMN utr text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ALTER COLUMN payout_customer_id SET DEFAULT null;
ALTER TABLE atlas_driver_offer_bpp.pg_payout_settlement_report ALTER COLUMN payout_customer_id DROP NOT NULL;