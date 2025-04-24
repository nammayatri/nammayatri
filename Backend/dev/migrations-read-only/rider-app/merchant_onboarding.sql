CREATE TABLE atlas_app.merchant_onboarding ();

ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN description text ;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN onboarding_type text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN remarks text ;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN requestor_id text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.merchant_onboarding ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_onboarding ADD PRIMARY KEY ( id, requestor_id);



------- SQL updates -------
------- already ran in master the next three updates ------------
ALTER TABLE atlas_app.merchant_onboarding DROP CONSTRAINT merchant_onboarding_pkey;
ALTER TABLE atlas_app.merchant_onboarding ADD PRIMARY KEY ( id);
ALTER TABLE atlas_app.merchant_onboarding ADD CONSTRAINT merchant_onboarding_unique_idx_onboarding_type_requestor_id UNIQUE (onboarding_type, requestor_id);