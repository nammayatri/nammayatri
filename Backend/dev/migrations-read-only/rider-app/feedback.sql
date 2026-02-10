CREATE TABLE atlas_app.feedback ();

ALTER TABLE atlas_app.feedback ADD COLUMN badge character varying (255) NOT NULL;
ALTER TABLE atlas_app.feedback ADD COLUMN badge_key character varying (255) ;
ALTER TABLE atlas_app.feedback ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.feedback ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.feedback ADD COLUMN rating integer ;
ALTER TABLE atlas_app.feedback ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.feedback ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.feedback ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.feedback ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.feedback ADD PRIMARY KEY ( id);
