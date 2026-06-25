CREATE TABLE atlas_app.reward_cohort ();

ALTER TABLE atlas_app.reward_cohort ADD COLUMN campaign_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN coupon_validity_days integer ;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN description text ;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN display_order integer NOT NULL;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN eligibility_json_logic jsonb NOT NULL;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN reward_image_url text ;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN reward_title text NOT NULL;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.reward_cohort ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.reward_cohort ALTER COLUMN eligibility_json_logic TYPE jsonb USING eligibility_json_logic::jsonb;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.reward_cohort ADD COLUMN presentation jsonb ;


------- SQL updates -------

ALTER TABLE atlas_app.reward_cohort ALTER COLUMN presentation TYPE jsonb USING CASE WHEN presentation IS NULL OR presentation = '' THEN NULL ELSE presentation::jsonb END;


------- SQL updates -------

ALTER TABLE atlas_app.reward_cohort ALTER COLUMN eligibility_json_logic TYPE jsonb USING eligibility_json_logic::jsonb;
ALTER TABLE atlas_app.reward_cohort ALTER COLUMN presentation TYPE jsonb USING CASE WHEN presentation IS NULL OR presentation = '' THEN NULL ELSE presentation::jsonb END;


------- SQL updates -------

ALTER TABLE atlas_app.reward_cohort ALTER COLUMN presentation TYPE jsonb;
ALTER TABLE atlas_app.reward_cohort ALTER COLUMN eligibility_json_logic TYPE jsonb;