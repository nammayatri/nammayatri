CREATE TABLE atlas_app.wallet_history ();

ALTER TABLE atlas_app.wallet_history ADD COLUMN benefit_value double precision ;
ALTER TABLE atlas_app.wallet_history ADD COLUMN campaign_id text ;
ALTER TABLE atlas_app.wallet_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.wallet_history ADD COLUMN domain_entity_id text NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN kind text NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_app.wallet_history ADD COLUMN points double precision NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN program_type text NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.wallet_history ADD COLUMN wallet_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD COLUMN wallet_payments_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet_history ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.wallet_history ADD COLUMN reversed_points double precision ;