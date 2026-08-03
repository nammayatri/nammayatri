CREATE TABLE atlas_app.finance_ref_type_config ();

ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN commission_value_amount double precision ;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN commission_value_type text ;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN direct_tax_rates json ;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN enabled boolean NOT NULL default true;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN indirect_tax_direction text ;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN is_tax_exclusive boolean NOT NULL default false;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN reference_type text NOT NULL;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN tax_rate_type text ;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN tax_rate_value double precision ;
ALTER TABLE atlas_app.finance_ref_type_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.finance_ref_type_config ADD PRIMARY KEY ( id);
