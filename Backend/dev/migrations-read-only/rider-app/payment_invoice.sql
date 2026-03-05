CREATE TABLE atlas_app.payment_invoice ();

ALTER TABLE atlas_app.payment_invoice ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN invoice_number text NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN invoice_type text NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN payment_instrument text NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN payment_order_id character varying(36) ;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN payment_purpose text NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN payment_status text NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_invoice ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payment_invoice ADD PRIMARY KEY ( id);





------- SQL updates -------

CREATE INDEX payment_invoice_idx_invoice_number ON atlas_app.payment_invoice USING btree (invoice_number);
CREATE INDEX payment_invoice_idx_payment_order_id ON atlas_app.payment_invoice USING btree (payment_order_id);
CREATE INDEX payment_invoice_idx_ride_id ON atlas_app.payment_invoice USING btree (ride_id);


------- SQL updates -------

CREATE INDEX payment_invoice_idx_created_at ON atlas_app.payment_invoice USING btree (created_at);