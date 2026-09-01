CREATE TABLE atlas_app.payout_batch ();

ALTER TABLE atlas_app.payout_batch ADD COLUMN client_ref_no text NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_batch ADD COLUMN failure_reason text ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD COLUMN inquiry_attempts_today integer NOT NULL default 0;
ALTER TABLE atlas_app.payout_batch ADD COLUMN inquiry_quota_date date ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN item_count integer NOT NULL default 0;
ALTER TABLE atlas_app.payout_batch ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD COLUMN next_inquiry_at timestamp with time zone ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN origin text NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD COLUMN partner_batch_ref text ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN partner_response_code text ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN payout_rail text NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD COLUMN pending_count integer NOT NULL default 0;
ALTER TABLE atlas_app.payout_batch ADD COLUMN processed_count integer NOT NULL default 0;
ALTER TABLE atlas_app.payout_batch ADD COLUMN rejected_count integer NOT NULL default 0;
ALTER TABLE atlas_app.payout_batch ADD COLUMN resolved_at timestamp with time zone ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN retry_of_batch_id character varying(36) ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN run_id text ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD COLUMN submitted_at timestamp with time zone ;
ALTER TABLE atlas_app.payout_batch ADD COLUMN total_amount double precision NOT NULL default 0;
ALTER TABLE atlas_app.payout_batch ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_batch ADD COLUMN value_date date NOT NULL;
ALTER TABLE atlas_app.payout_batch ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.payout_batch ADD COLUMN merchant_operating_city_id text ;