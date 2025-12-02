CREATE TABLE atlas_driver_offer_bpp.wallet_reward_posting ();

ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN cash_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN points_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN posting_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD COLUMN wallet_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.wallet_reward_posting ADD PRIMARY KEY ( id);
