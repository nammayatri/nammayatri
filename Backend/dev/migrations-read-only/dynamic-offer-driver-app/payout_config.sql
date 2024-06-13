CREATE TABLE atlas_driver_offer_bpp.payout_config ();

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN payout_registration_amount double precision NOT NULL default 1.0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN referral_reward_amount_per_ride double precision NOT NULL default 100.0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD PRIMARY KEY ( merchant_operating_city_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN time_diff integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN threshold_payout_amount_per_person double precision NOT NULL default 1000.0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN is_payout_enabled boolean NOT NULL default True;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN batch_limit integer NOT NULL default Int;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ALTER COLUMN batch_limit SET DEFAULT 10;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN vehicle_service_tier text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ALTER COLUMN vehicle_service_tier DROP NOT NULL;