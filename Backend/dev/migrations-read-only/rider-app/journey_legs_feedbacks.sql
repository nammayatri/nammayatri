CREATE TABLE atlas_app.journey_legs_feedbacks ();

ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN is_experience_good boolean ;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN journey_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN leg_order integer NOT NULL;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.journey_legs_feedbacks ADD PRIMARY KEY ( journey_id, leg_order);



------- SQL updates -------

ALTER TABLE atlas_app.journey_legs_feedbacks ADD COLUMN travel_mode text ;