CREATE TABLE atlas_app.shuttle_interest ();

ALTER TABLE atlas_app.shuttle_interest ADD COLUMN company_domain text NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN days_of_week text[] NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN drop_address text NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN drop_lat double precision NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN drop_lon double precision NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN drop_time time ;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pass_organization_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pickup_address text NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pickup_lat double precision NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pickup_lon double precision NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pickup_time time ;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN trip_type text NOT NULL;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.shuttle_interest ADD PRIMARY KEY ( person_id);



------- SQL updates -------

ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pickup_location_source text ;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN pickup_accuracy_meters double precision ;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN drop_location_source text ;
ALTER TABLE atlas_app.shuttle_interest ADD COLUMN drop_accuracy_meters double precision ;