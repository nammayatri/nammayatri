CREATE TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ();

ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN assessment_year character varying(20) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN cert_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN cert_number character varying(64) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN document_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN fleet_owner_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN quarter character varying(10) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN rejection_reason text ;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN status character varying(20) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN tan_number character varying(20) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN tds_rate double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN tds_section text ;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ALTER COLUMN assessment_year TYPE text;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.finance_tds_reimbursement_request ALTER COLUMN document_id TYPE character varying(36);