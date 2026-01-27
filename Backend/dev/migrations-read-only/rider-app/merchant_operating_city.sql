CREATE TABLE atlas_app.merchant_operating_city ();

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN merchant_short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.merchant_operating_city ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN state text NOT NULL default 'Karnataka';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN long double precision NOT NULL default 0.0;
ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN lat double precision NOT NULL default 0.0;


------- SQL updates -------

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN country text NOT NULL default 'India';


------- SQL updates -------

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN driver_offer_merchant_operating_city_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.merchant_operating_city ADD COLUMN std_code text ;