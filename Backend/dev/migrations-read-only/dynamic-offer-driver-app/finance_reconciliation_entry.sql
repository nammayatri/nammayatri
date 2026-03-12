CREATE TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ();

ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN actual_ledger_value double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN booking_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN dco_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN expected_dsr_value double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN finance_component text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN mismatch_reason text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN mode text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN recon_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN reconciliation_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN reconciliation_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN source_details text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN summary_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN target_details text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN timestamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN variance double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN merchant_id TYPE text;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN merchant_operating_city_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN status SET DEFAULT 'COMPLETED';
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN status DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN mode SET DEFAULT 'ONLINE';
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN dco_id SET DEFAULT '';
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN dco_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN booking_id SET DEFAULT '';
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ALTER COLUMN booking_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN transaction_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN target_id text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN source_id text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN settlement_mode text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN settlement_id text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN settlement_date timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_entry ADD COLUMN rrn text ;