CREATE TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ();

ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN card_isin character varying(10) ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN card_network text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN card_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN dispute_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN dispute_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN order_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN payment_gateway text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN payment_method text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN payment_method_sub_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN pg_base_fee double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN pg_tax double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN raw_data text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN recon_message text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN recon_status text NOT NULL default 'PENDING';
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN reference_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN reference_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_arn text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_base_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_tax double precision ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN rrn text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN settlement_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN settlement_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN settlement_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN settlement_mode text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN settlement_type text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN txn_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN txn_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN txn_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN txn_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN txn_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN unique_split_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN utr text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN vendor_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD PRIMARY KEY ( id);



------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_reason_code text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN refund_method text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN pg_approval_code text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN chargeback_status text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN chargeback_reason_code text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN chargeback_id text ;
ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN bank_id text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.pg_payment_settlement_report ADD COLUMN chargeback_amount double precision ;