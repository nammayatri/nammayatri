CREATE TABLE atlas_app.recent_location ();

ALTER TABLE atlas_app.recent_location ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.recent_location ADD COLUMN route_id text ;
ALTER TABLE atlas_app.recent_location ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.recent_location ADD PRIMARY KEY ( id);
