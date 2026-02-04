CREATE TABLE atlas_app.finance_invoice_ledger_link ();

ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN invoice_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN ledger_entry_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_invoice_ledger_link ADD PRIMARY KEY ( id);
