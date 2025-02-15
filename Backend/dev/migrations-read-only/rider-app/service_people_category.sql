CREATE TABLE atlas_app.service_people_category ();

ALTER TABLE atlas_app.service_people_category ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN price_per_unit double precision NOT NULL;
ALTER TABLE atlas_app.service_people_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.service_people_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.service_people_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.service_people_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.service_people_category ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.service_people_category ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.service_people_category ADD COLUMN cancellation_charges json ;


------- SQL updates -------

ALTER TABLE atlas_app.service_people_category ADD COLUMN time_bounds text  default 'Unbounded';


------- SQL updates -------

ALTER TABLE atlas_app.service_people_category ADD COLUMN pricing_type text  default 'AllDays';


------- SQL updates -------

ALTER TABLE atlas_app.service_people_category ADD COLUMN vendor_split_details json ;