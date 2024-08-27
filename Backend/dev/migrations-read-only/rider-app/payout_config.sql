CREATE TABLE atlas_app.payout_config ();

ALTER TABLE atlas_app.payout_config ADD COLUMN batch_limit integer NOT NULL default 10;
ALTER TABLE atlas_app.payout_config ADD COLUMN is_payout_enabled boolean NOT NULL default False;
ALTER TABLE atlas_app.payout_config ADD COLUMN max_retry_count integer NOT NULL default 5;
ALTER TABLE atlas_app.payout_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_config ADD COLUMN order_type text NOT NULL default 'FULFILL_ONLY';
ALTER TABLE atlas_app.payout_config ADD COLUMN payout_entity text NOT NULL;
ALTER TABLE atlas_app.payout_config ADD COLUMN remark text NOT NULL default 'Cashback for metro ticket booking with NammaYatri';
ALTER TABLE atlas_app.payout_config ADD COLUMN time_diff integer NOT NULL default 86400;
ALTER TABLE atlas_app.payout_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_config ADD PRIMARY KEY ( merchant_operating_city_id);
