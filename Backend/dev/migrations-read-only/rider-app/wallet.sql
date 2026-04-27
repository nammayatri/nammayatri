CREATE TABLE atlas_app.wallet ();

ALTER TABLE atlas_app.wallet ADD COLUMN account_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN available_balance double precision NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN cashback_earned double precision NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.wallet ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN current_available_points double precision NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN lifetime_burned double precision NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN lifetime_earned double precision NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_app.wallet ADD COLUMN person_id text NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN program_id text NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN program_type text NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN topup_earned double precision NOT NULL;
ALTER TABLE atlas_app.wallet ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.wallet ADD PRIMARY KEY ( id);
