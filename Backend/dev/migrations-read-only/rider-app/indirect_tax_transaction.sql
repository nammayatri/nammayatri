CREATE TABLE atlas_app.indirect_tax_transaction ();

ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN cgst_amount double precision NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN counterparty_id text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN credit_or_debit_note_number text ;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN gst_credit_type text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN gst_rate double precision NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN gstin_of_party text ;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN igst_amount double precision NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN invoice_number text ;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN reference_id text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN sac_code text ;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN sale_type text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN sgst_amount double precision NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN taxable_value double precision NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN total_gst_amount double precision NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN transaction_date timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN transaction_type text NOT NULL;
ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.indirect_tax_transaction ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.indirect_tax_transaction ADD COLUMN external_charges double precision ;