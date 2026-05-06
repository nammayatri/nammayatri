CREATE TABLE atlas_app.settlement_file_info ();

ALTER TABLE atlas_app.settlement_file_info ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN file_name text NOT NULL;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN last_processed_index integer NOT NULL default -1;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN payment_gateway_name text NOT NULL;
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN status text NOT NULL default 'PENDING';
ALTER TABLE atlas_app.settlement_file_info ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.settlement_file_info ADD PRIMARY KEY ( id);
