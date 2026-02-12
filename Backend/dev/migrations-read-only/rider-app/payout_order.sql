CREATE TABLE atlas_app.payout_order ();

ALTER TABLE atlas_app.payout_order ADD COLUMN account_details_type text ;
ALTER TABLE atlas_app.payout_order ADD COLUMN currency text ;
ALTER TABLE atlas_app.payout_order ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_order ADD COLUMN customer_email_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN customer_email_hash bytea NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN customer_id text NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN entity_id text ;
ALTER TABLE atlas_app.payout_order ADD COLUMN entity_name text ;
ALTER TABLE atlas_app.payout_order ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN last_status_checked_at timestamp with time zone ;
ALTER TABLE atlas_app.payout_order ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN mobile_no_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN mobile_no_hash bytea NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN order_id text NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.payout_order ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_order ADD COLUMN vpa text ;
ALTER TABLE atlas_app.payout_order ADD PRIMARY KEY ( id, order_id);


------- SQL updates -------

ALTER TABLE atlas_app.payout_order ADD COLUMN entity_ids text[] ;



------- SQL updates -------

ALTER TABLE atlas_app.payout_order ADD COLUMN short_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.payout_order ADD COLUMN response_message text ;
ALTER TABLE atlas_app.payout_order ADD COLUMN response_code text ;


------- SQL updates -------

ALTER TABLE atlas_app.payout_order ADD COLUMN retried_order_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.payout_order ADD COLUMN merchant_operating_city_id text ;



------- SQL updates -------

ALTER TABLE atlas_app.payout_order ADD COLUMN payout_service_type text ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

