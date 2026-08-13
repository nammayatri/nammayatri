CREATE TABLE atlas_driver_offer_bpp.recon_settlement_order ();

ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN allocated_bank_cash numeric(30,2)  default 0.0;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN bff_amount numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN bff_type text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN claimed_gross_amount numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN claimed_settlement_amount numeric(30,2) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN correction_for_order_row_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN deduction_by_collector numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN diff_amount numeric(30,2)  default 0.0;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN driver_id text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN invoice_no text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN manual_confirmation_reason text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN manually_confirmed_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN manually_confirmed_by text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN merchant_id text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN message_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN order_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN order_state text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN order_transaction_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN our_recon_status character varying(50) NOT NULL default 'PENDING';
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN payment_status character varying(50) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN platform_gross_fare numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN platform_net_receivable numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN raw_json text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN reason_code character varying(10) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN received_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN recon_transaction_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN refund_status character varying(50) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN ride_id text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN settlement_cleared_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN settlement_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN settlement_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN settlement_reference_no text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN settlement_type character varying(50) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN source_type character varying(50)  default 'BAP_CLAIMED';
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN utr_settlement_id character varying(36)  default NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN wire_order_recon_status character varying(50) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN wire_recon_status character varying(50) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN withholding_tax_gst numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN withholding_tax_tds numeric(30,2) ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ALTER COLUMN diff_amount DROP DEFAULT;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ALTER COLUMN allocated_bank_cash DROP DEFAULT;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN remarks text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN refunded_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN refund_reference text ;
ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN reconciliation_status text ;

------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD COLUMN platform_order_timestamp timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.recon_settlement_order ADD CONSTRAINT recon_settlement_order_unique_idx_order_id_settlement_reference_no UNIQUE (order_id, settlement_reference_no);


------- SQL updates -------




------- SQL updates -------

CREATE INDEX CONCURRENTLY recon_settlement_order_idx_message_id ON atlas_driver_offer_bpp.recon_settlement_order USING btree (message_id);
CREATE INDEX CONCURRENTLY recon_settlement_order_idx_order_id ON atlas_driver_offer_bpp.recon_settlement_order USING btree (order_id);
CREATE INDEX CONCURRENTLY recon_settlement_order_idx_ride_id ON atlas_driver_offer_bpp.recon_settlement_order USING btree (ride_id);
CREATE INDEX CONCURRENTLY recon_settlement_order_idx_utr_settlement_id ON atlas_driver_offer_bpp.recon_settlement_order USING btree (utr_settlement_id);