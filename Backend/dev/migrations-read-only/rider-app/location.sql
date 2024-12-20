CREATE TABLE atlas_app.location ();

ALTER TABLE atlas_app.location ADD COLUMN area text ;
ALTER TABLE atlas_app.location ADD COLUMN area_code text ;
ALTER TABLE atlas_app.location ADD COLUMN building text ;
ALTER TABLE atlas_app.location ADD COLUMN city text ;
ALTER TABLE atlas_app.location ADD COLUMN country text ;
ALTER TABLE atlas_app.location ADD COLUMN door text ;
ALTER TABLE atlas_app.location ADD COLUMN place_id text ;
ALTER TABLE atlas_app.location ADD COLUMN state text ;
ALTER TABLE atlas_app.location ADD COLUMN street text ;
ALTER TABLE atlas_app.location ADD COLUMN ward text ;
ALTER TABLE atlas_app.location ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.location ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.location ADD COLUMN lat double precision NOT NULL;
ALTER TABLE atlas_app.location ADD COLUMN lon double precision NOT NULL;
ALTER TABLE atlas_app.location ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.location ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.location ADD COLUMN instructions text ;
ALTER TABLE atlas_app.location ADD COLUMN extras text ;


------- SQL updates -------

ALTER TABLE atlas_app.location ADD COLUMN title text ;


------- SQL updates -------

ALTER TABLE atlas_app.location ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.location ADD COLUMN merchant_id character varying(36) ;