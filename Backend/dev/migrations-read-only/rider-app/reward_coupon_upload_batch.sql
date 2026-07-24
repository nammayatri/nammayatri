CREATE TABLE atlas_app.reward_coupon_upload_batch ();

ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN campaign_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN cohort_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN s3_key text NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN total_codes_uploaded integer NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN upload_batch_id text NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN uploaded_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN uploaded_by text NOT NULL;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_coupon_upload_batch ADD PRIMARY KEY ( id);
