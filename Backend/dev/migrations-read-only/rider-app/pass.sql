CREATE TABLE atlas_app.pass ();

ALTER TABLE atlas_app.pass ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN applicable_vehicle_service_tiers text[] NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN auto_apply boolean NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN benefit text ;
ALTER TABLE atlas_app.pass ADD COLUMN benefit_description text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN code text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN documents_required text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN enable boolean NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN max_valid_days integer ;
ALTER TABLE atlas_app.pass ADD COLUMN max_valid_trips integer ;
ALTER TABLE atlas_app.pass ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN name text ;
ALTER TABLE atlas_app.pass ADD COLUMN "order" integer NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN pass_type_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN purchase_eligibility_json_logic text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN redeem_eligibility_json_logic text NOT NULL;
ALTER TABLE atlas_app.pass ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pass ADD PRIMARY KEY ( id);

------- SQL updates -------


-- Fix column types: convert from text to arrays
ALTER TABLE atlas_app.pass
  ALTER COLUMN documents_required TYPE text[] USING '{}';

ALTER TABLE atlas_app.pass
  ALTER COLUMN purchase_eligibility_json_logic TYPE jsonb[] USING '{}';

ALTER TABLE atlas_app.pass
  ALTER COLUMN redeem_eligibility_json_logic TYPE jsonb[] USING '{}';


------- SQL updates -------

