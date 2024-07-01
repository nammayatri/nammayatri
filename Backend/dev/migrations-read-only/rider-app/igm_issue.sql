CREATE TABLE atlas_app.igm_issue ();

ALTER TABLE atlas_app.igm_issue ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.igm_issue ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.igm_issue ADD COLUMN internal_issue_id text NOT NULL;
ALTER TABLE atlas_app.igm_issue ADD COLUMN issue_status text NOT NULL default 'OPEN';
ALTER TABLE atlas_app.igm_issue ADD COLUMN respondent_email text ;
ALTER TABLE atlas_app.igm_issue ADD COLUMN respondent_name text ;
ALTER TABLE atlas_app.igm_issue ADD COLUMN respondent_phone text ;
ALTER TABLE atlas_app.igm_issue ADD COLUMN responding_merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.igm_issue ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.igm_issue ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.igm_issue ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.igm_issue ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ALTER COLUMN issue_status DROP DEFAULT;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ADD COLUMN issue_type text NOT NULL;
ALTER TABLE atlas_app.igm_issue DROP COLUMN type;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ADD COLUMN respondent_action text ;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ADD COLUMN respondent_entity_type text ;



------- SQL updates -------

ALTER TABLE atlas_app.igm_issue DROP COLUMN internal_issue_id;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ADD COLUMN rider_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ADD COLUMN transaction_id text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ADD COLUMN booking_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue ALTER COLUMN responding_merchant_id TYPE text;
ALTER TABLE atlas_app.igm_issue ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.igm_issue DROP COLUMN merchant_id;