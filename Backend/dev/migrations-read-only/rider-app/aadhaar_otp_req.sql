CREATE TABLE atlas_app.aadhaar_otp_req ();

ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN request_id text NOT NULL;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN request_message text NOT NULL;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN status_code text NOT NULL;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN transaction_id text ;
ALTER TABLE atlas_app.aadhaar_otp_req ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.aadhaar_otp_req ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.aadhaar_otp_req ALTER COLUMN updated_at DROP NOT NULL;