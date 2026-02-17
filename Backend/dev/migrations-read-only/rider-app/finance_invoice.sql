CREATE TABLE atlas_app.finance_invoice ();

ALTER TABLE atlas_app.finance_invoice ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN due_at timestamp with time zone ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN invoice_number text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN invoice_type text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_by_id text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_by_name text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_by_type text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_to_id text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_to_name text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_to_type text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN line_items jsonb NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN subtotal double precision NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN tax_breakdown text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN total_amount double precision NOT NULL;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_invoice ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.finance_invoice ADD COLUMN payment_order_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_to_address text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN issued_by_address text ;


------- SQL updates -------

ALTER TABLE atlas_app.finance_invoice ADD COLUMN supplier_name text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN supplier_id text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN supplier_gstin text ;
ALTER TABLE atlas_app.finance_invoice ADD COLUMN supplier_address text ;