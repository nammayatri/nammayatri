CREATE TABLE atlas_app.issue ();

ALTER TABLE atlas_app.issue ADD COLUMN booking_id character varying(36) ;
ALTER TABLE atlas_app.issue ADD COLUMN contact_email text ;
ALTER TABLE atlas_app.issue ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.issue ADD COLUMN customer_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.issue ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.issue ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.issue ADD COLUMN night_safety boolean NOT NULL default false;
ALTER TABLE atlas_app.issue ADD COLUMN reason text NOT NULL;
ALTER TABLE atlas_app.issue ADD COLUMN status text NOT NULL default 'OPEN';
ALTER TABLE atlas_app.issue ADD COLUMN ticket_id text ;
ALTER TABLE atlas_app.issue ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.issue ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.issue ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.issue ADD COLUMN backn_issue_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.issue ADD COLUMN beckn_issue_id text ;
ALTER TABLE atlas_app.issue DROP COLUMN backn_issue_id;


------- SQL updates -------

ALTER TABLE atlas_app.issue DROP COLUMN beckn_issue_id;