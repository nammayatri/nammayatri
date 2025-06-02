CREATE TABLE atlas_app.business_hour ();

ALTER TABLE atlas_app.business_hour ADD COLUMN btype text NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN category_id text[] NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.business_hour ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.business_hour ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.business_hour ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.business_hour ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.business_hour ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.business_hour ADD COLUMN booking_closing_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.business_hour ALTER COLUMN booking_closing_time TYPE time without time zone;


------- SQL updates -------

ALTER TABLE atlas_app.business_hour ADD COLUMN place_id text ;
ALTER TABLE atlas_app.business_hour ADD COLUMN name text ;


------- SQL updates -------

ALTER TABLE atlas_app.business_hour ADD COLUMN hash text ;


------- SQL updates -------

ALTER TABLE atlas_app.business_hour ADD COLUMN expiry_date date ;