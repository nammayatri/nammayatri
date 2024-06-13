CREATE TABLE atlas_driver_offer_bpp.driver_profile_questions ();

ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN aspirations text[] NOT NULL default [];
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN expert_at text[] NOT NULL default [];
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN hometown text ;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN pledges text[] NOT NULL default [];
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD COLUMN why_ny text[] NOT NULL default [];
ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions ADD PRIMARY KEY ( driver_id);