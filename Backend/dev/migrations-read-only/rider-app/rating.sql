CREATE TABLE atlas_app.rating ();

ALTER TABLE atlas_app.rating ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rating ADD COLUMN feedback_details text ;
ALTER TABLE atlas_app.rating ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rating ADD COLUMN rating_value integer NOT NULL;
ALTER TABLE atlas_app.rating ADD COLUMN ride_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rating ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.rating ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.rating ADD COLUMN was_offered_assistance boolean ;
ALTER TABLE atlas_app.rating ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.rating ADD COLUMN media_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.rating ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.rating ADD COLUMN merchant_id character varying(36) ;