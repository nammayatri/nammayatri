CREATE TABLE atlas_app.journal_entry_transaction ();

ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN created_by text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN credit_amount double precision NOT NULL default 0;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN debit_amount double precision NOT NULL default 0;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN sap_batch_id text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN sap_journal_entry_id text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN subscription_id text ;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN transaction_type text NOT NULL;
ALTER TABLE atlas_app.journal_entry_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journal_entry_transaction ADD PRIMARY KEY ( id);
