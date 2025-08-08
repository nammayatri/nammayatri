CREATE TABLE atlas_app.pickup_instructions ();

ALTER TABLE atlas_app.pickup_instructions ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN geohash character(12) NOT NULL;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN instruction character(150) NOT NULL;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN media_file_id character(36) ;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN merchant_id character(36) NOT NULL;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN merchant_operating_city_id character(36) NOT NULL;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN person_id character(36) NOT NULL;
ALTER TABLE atlas_app.pickup_instructions ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pickup_instructions ADD PRIMARY KEY ( id);