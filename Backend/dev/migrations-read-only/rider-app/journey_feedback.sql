CREATE TABLE atlas_app.journey_feedback ();

ALTER TABLE atlas_app.journey_feedback ADD COLUMN additional_feed_back text ;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN rating integer ;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_feedback ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_feedback ADD PRIMARY KEY ( journey_id);
