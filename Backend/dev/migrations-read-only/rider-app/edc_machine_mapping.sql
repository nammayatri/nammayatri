CREATE TABLE atlas_app.edc_machine_mapping ();

ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN client_id text NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN created_by character varying(36) ;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN is_active boolean NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN machine_name text ;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN merchant_channel_id text NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN merchant_key text NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN paytm_mid text NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN terminal_id text NOT NULL;
ALTER TABLE atlas_app.edc_machine_mapping ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.edc_machine_mapping ADD PRIMARY KEY ( id);
