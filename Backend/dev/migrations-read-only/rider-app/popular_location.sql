CREATE TABLE atlas_app.popular_location ();

ALTER TABLE atlas_app.popular_location ADD COLUMN address text NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.popular_location ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN rating double precision ;
ALTER TABLE atlas_app.popular_location ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.popular_location ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.popular_location ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.popular_location ADD PRIMARY KEY ( id);
