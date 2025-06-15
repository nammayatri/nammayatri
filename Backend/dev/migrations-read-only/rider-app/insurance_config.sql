CREATE TABLE atlas_app.insurance_config ();

ALTER TABLE atlas_app.insurance_config ADD COLUMN allowed_vehicle_service_tiers text[] ;
ALTER TABLE atlas_app.insurance_config ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.insurance_config ADD COLUMN hours integer NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN partner_id text NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN plan text NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN plan_type integer ;
ALTER TABLE atlas_app.insurance_config ADD COLUMN state text NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN trip_category text NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.insurance_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_app.insurance_config ADD PRIMARY KEY ( id);
ALTER TABLE atlas_app.insurance_config ADD CONSTRAINT insurance_config_unique_idx_merchant_id_merchant_operating_city_id_trip_category_vehicle_category UNIQUE (merchant_id, merchant_operating_city_id, trip_category, vehicle_category);


------- SQL updates -------

ALTER TABLE atlas_app.insurance_config ADD COLUMN insured_amount text ;


------- SQL updates -------

ALTER TABLE atlas_app.insurance_config ADD COLUMN driver_insured_amount text ;