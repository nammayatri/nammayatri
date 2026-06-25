CREATE TABLE atlas_app.reward_campaign ();

ALTER TABLE atlas_app.reward_campaign ADD COLUMN claim_mode text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN coupon_source_type text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN coupon_template text ;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN created_by text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN description text ;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN display_order integer NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN ends_at timestamp with time zone ;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN reclaim_policy jsonb ;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN redemption_target_type text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN redemption_target_url text ;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN sponsor_logo_url text ;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN sponsor_name text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN sponsor_type text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN starts_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.reward_campaign ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_campaign ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.reward_campaign ALTER COLUMN status SET DEFAULT 'Draft';
ALTER TABLE atlas_app.reward_campaign ALTER COLUMN display_order SET DEFAULT 0;
ALTER TABLE atlas_app.reward_campaign ALTER COLUMN claim_mode SET DEFAULT 'AutoClaim';


------- SQL updates -------

ALTER TABLE atlas_app.reward_campaign ALTER COLUMN reclaim_policy TYPE jsonb USING CASE WHEN reclaim_policy IS NULL OR reclaim_policy = '' THEN NULL ELSE reclaim_policy::jsonb END;


------- SQL updates -------

ALTER TABLE atlas_app.reward_campaign ALTER COLUMN reclaim_policy TYPE jsonb;