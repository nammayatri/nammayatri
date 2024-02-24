CREATE TABLE atlas_driver_offer_bpp.question_information ();

ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN options json NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN question text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN question_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN question_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.question_information ADD PRIMARY KEY ( language, question_id);