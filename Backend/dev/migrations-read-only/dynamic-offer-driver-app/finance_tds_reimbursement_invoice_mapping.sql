CREATE TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ();

ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN invoice_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN revenue_recognised_snapshot double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN tds_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD COLUMN tds_credit_receivable double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_invoice_mapping ADD PRIMARY KEY ( id);
