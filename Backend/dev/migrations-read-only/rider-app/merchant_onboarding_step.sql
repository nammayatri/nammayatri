CREATE TABLE atlas_app.merchant_onboarding_step ();

ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN dependency text[] NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN is_approval_required boolean NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN merchant_onboarding_id text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN payload json ;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN remarks text ;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN step_description text ;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN step_name_identifier text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_onboarding_step ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.merchant_onboarding_step ADD COLUMN is_admin_only boolean ;