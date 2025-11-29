-- NOTE: This file is auto-generated.
-- Parts of this migration (table creation and some columns) might have already been applied via 0205-add-feedback-badges.sql.
-- If the table 'atlas_driver_offer_bpp.feedback_badge' already exists, DO NOT run the CREATE TABLE statement.
-- Only run the ALTER TABLE ADD COLUMN statements for the following new columns that do not exist yet:
-- badge_key, merchant_id, merchant_operating_city_id

CREATE TABLE atlas_driver_offer_bpp.feedback_badge ();

ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN badge character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN badge_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN badge_key character varying (255) ;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.feedback_badge ADD PRIMARY KEY ( id);
