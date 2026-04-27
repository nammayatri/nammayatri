CREATE TABLE atlas_app.wallet_payments ();

ALTER TABLE atlas_app.wallet_payments ADD COLUMN campaign_id text ;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN kind text NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN order_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN person_id text NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN points double precision NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN program_id text NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.wallet_payments ADD COLUMN wallet_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet_payments ADD PRIMARY KEY ( id);
