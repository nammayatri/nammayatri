CREATE TABLE atlas_driver_offer_bpp.driver_profile_questions ();

ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN aspirations text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN expert_at text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN hometown text ;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN pledges text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN why_ny text[] NOT NULL default '{}';
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ALTER COLUMN aspirations DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN vehicle_tags text[]  default '{}';
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN profile_image_id text ;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN other_images text[] ;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN driving_since integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN about_me text ;

--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ALTER COLUMN why_ny DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ALTER COLUMN merchant_id DROP NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ALTER COLUMN expert_at DROP NOT NULL;
--- Drop section ends. Please check before running ---



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN image_ids text[]  default '{}';