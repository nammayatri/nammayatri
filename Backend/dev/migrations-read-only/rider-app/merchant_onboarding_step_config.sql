CREATE TABLE atlas_app.merchant_onboarding_step_config ();

ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN dependency text[] NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN is_approval_required boolean NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN onboarding_type text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN step_description text ;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN step_name_identifier text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_onboarding_step_config ADD PRIMARY KEY ( onboarding_type, step_name_identifier);



------- SQL updates -------

ALTER TABLE atlas_app.merchant_onboarding_step_config ADD COLUMN is_admin_only boolean ;