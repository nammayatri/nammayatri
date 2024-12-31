CREATE TABLE atlas_app.pass ();

ALTER TABLE atlas_app.pass ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN benefit text ;
ALTER TABLE atlas_app.pass ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN days integer ;
ALTER TABLE atlas_app.pass ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN pass_type_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN purchase_eligibility_json_logic text[] NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN redeem_eligibility_json_logic text[] NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN savings double precision ;
ALTER TABLE atlas_app.pass ADD COLUMN trips integer ;
ALTER TABLE atlas_app.pass ADD COLUMN vehicle_service_tier_type text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.pass ADD COLUMN name text ;