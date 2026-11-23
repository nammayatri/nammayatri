CREATE TABLE atlas_app.shuttle_published_trip ();

ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN day text NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN departure text NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN feed_key text NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN riders integer NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN trip_id text NOT NULL;
ALTER TABLE atlas_app.shuttle_published_trip ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shuttle_published_trip ADD PRIMARY KEY ( day, trip_id);
