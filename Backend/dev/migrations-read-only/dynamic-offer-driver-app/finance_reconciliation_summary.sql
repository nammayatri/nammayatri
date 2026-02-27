CREATE TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ();

ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN error_message text ;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN match_rate text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN matched_records integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN reconciliation_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN reconciliation_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN source_total double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN target_total double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN total_discrepancies integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD COLUMN variance_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_reconciliation_summary ADD PRIMARY KEY ( id);
