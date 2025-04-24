CREATE TABLE atlas_app.ticket_merchant_details ();

ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN agreement_letter_encrypted text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN agreement_letter_hash text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN bank_account_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN bank_account_number_hash bytea NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN bank_account_type text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN bank_beneficiary_name text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN bank_ifsc_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN bank_ifsc_hash bytea NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN contact_details_email text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN contact_details_name text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN contact_details_number text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN doc_cancelled_cheque_encrypted text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN doc_cancelled_cheque_hash text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN doc_pan_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN doc_pan_hash bytea NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN gstin_encrypted text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN gstin_hash text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN org_address text ;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN org_name text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN pan_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN pan_hash bytea NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN state text NOT NULL;
ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_merchant_details ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.ticket_merchant_details ADD COLUMN is_bank_onboarded boolean ;