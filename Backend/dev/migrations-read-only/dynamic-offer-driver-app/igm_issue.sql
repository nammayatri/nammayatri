CREATE TABLE atlas_driver_offer_bpp.igm_issue ();

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN customer_email text ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN customer_name text ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN customer_phone text ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN internal_issue_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN issue_raised_by_merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN issue_raised_by_name text ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN issue_status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN issue_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN respondent_action text ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.igm_issue DROP COLUMN internal_issue_id;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.igm_issue DROP COLUMN issue_raised_by_name;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN resolution_action text ;
ALTER TABLE atlas_driver_offer_bpp.igm_issue ADD COLUMN booking_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.igm_issue ALTER COLUMN merchant_id SET NOT NULL;