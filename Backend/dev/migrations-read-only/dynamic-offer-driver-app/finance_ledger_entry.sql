CREATE TABLE atlas_driver_offer_bpp.finance_ledger_entry ();

ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN entry_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN from_account_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN from_ending_balance double precision ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN from_starting_balance double precision ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN metadata text ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN reference_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN reference_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN reversal_of character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN settled_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN timestamp timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN to_account_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN to_ending_balance double precision ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN to_starting_balance double precision ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN void_reason text ;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.finance_ledger_entry ADD COLUMN reconciliation_status text ;