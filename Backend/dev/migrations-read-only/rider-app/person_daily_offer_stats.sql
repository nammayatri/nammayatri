CREATE TABLE atlas_app.person_daily_offer_stats ();

ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN date date NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN offer_count integer NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN payout_status text NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN person_id text NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN total_cashback_amount double precision NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN total_discount_amount double precision NOT NULL;
ALTER TABLE atlas_app.person_daily_offer_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_daily_offer_stats ADD PRIMARY KEY ( id);
