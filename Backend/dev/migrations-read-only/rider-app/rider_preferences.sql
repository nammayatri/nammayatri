CREATE TABLE atlas_app.rider_preferences ();

ALTER TABLE atlas_app.rider_preferences ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN preference_data jsonb NOT NULL;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN preference_type character varying(255) NOT NULL;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rider_preferences ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rider_preferences ADD PRIMARY KEY ( id);
