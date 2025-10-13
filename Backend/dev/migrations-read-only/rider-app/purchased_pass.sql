CREATE TABLE atlas_app.purchased_pass ();

ALTER TABLE atlas_app.purchased_pass ADD COLUMN applicable_vehicle_service_tiers text[] NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN benefit text ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN benefit_description text NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN max_valid_days integer ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN max_valid_trips integer ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN order_short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN pass_amount double precision NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN pass_code text NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN pass_name text ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN pass_type_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN used_count integer NOT NULL;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN valid_till timestamp with time zone ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.purchased_pass ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.purchased_pass ADD COLUMN benefit_value double precision ;
ALTER TABLE atlas_app.purchased_pass ADD COLUMN benefit_type text ;


------- SQL updates -------

