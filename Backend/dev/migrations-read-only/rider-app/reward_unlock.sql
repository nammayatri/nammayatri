CREATE TABLE atlas_app.reward_unlock ();

ALTER TABLE atlas_app.reward_unlock ADD COLUMN campaign_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN claimed_at timestamp with time zone ;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN cohort_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN coupon_code text ;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN coupon_source text NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN coupon_valid_till timestamp with time zone ;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN reclaimed_at timestamp with time zone ;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN redeemed_at timestamp with time zone ;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN unlocked_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN viewed_at timestamp with time zone ;
ALTER TABLE atlas_app.reward_unlock ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.reward_unlock ALTER COLUMN status SET DEFAULT 'Active';
ALTER TABLE atlas_app.reward_unlock ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.reward_unlock ADD COLUMN merchant_id character varying(36) ;