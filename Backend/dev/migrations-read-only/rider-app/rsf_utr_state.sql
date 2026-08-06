CREATE TABLE atlas_app.rsf_utr_state ();

ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN last_reported_at timestamp with time zone ;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN last_reported_message_id text ;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN reported_diff numeric(30,2) ;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN reported_status text ;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rsf_utr_state ADD COLUMN utr text NOT NULL;
ALTER TABLE atlas_app.rsf_utr_state ADD PRIMARY KEY ( merchant_id, utr);
