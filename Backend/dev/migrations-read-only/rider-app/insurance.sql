CREATE TABLE atlas_app.insurance ();

ALTER TABLE atlas_app.insurance ADD COLUMN category text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN certificate_url text ;
ALTER TABLE atlas_app.insurance ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.insurance ADD COLUMN customer_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN customer_name text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN customer_phone text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN driver_name text ;
ALTER TABLE atlas_app.insurance ADD COLUMN driver_phone text ;
ALTER TABLE atlas_app.insurance ADD COLUMN end_date timestamp with time zone ;
ALTER TABLE atlas_app.insurance ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN fair_breakup text ;
ALTER TABLE atlas_app.insurance ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN partner_id text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN plan text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN policy_id text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN policy_number text NOT NULL;
ALTER TABLE atlas_app.insurance ADD COLUMN start_date timestamp with time zone ;
ALTER TABLE atlas_app.insurance ADD COLUMN trip_category text ;
ALTER TABLE atlas_app.insurance ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.insurance ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.insurance ADD COLUMN insured_amount text ;
ALTER TABLE atlas_app.insurance ADD COLUMN driver_insured_amount text ;