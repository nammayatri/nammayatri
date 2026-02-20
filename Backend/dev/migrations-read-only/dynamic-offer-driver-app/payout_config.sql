CREATE TABLE atlas_driver_offer_bpp.payout_config ();

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN batch_limit integer NOT NULL default 10;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN is_payout_enabled boolean NOT NULL default False;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN max_retry_count integer NOT NULL default 5;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN order_type text NOT NULL default 'FULFILL_ONLY';
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN payout_registration_cgst double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN payout_registration_fee double precision NOT NULL default 1.0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN payout_registration_sgst double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN referral_reward_amount_per_ride double precision NOT NULL default 100.0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN remark text NOT NULL default 'Referral Reward From Nammayatri';
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN threshold_payout_amount_per_person double precision NOT NULL default 1000.0;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN time_diff integer NOT NULL default 86400;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN vehicle_category text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.payout_config ADD PRIMARY KEY ( merchant_operating_city_id, vehicle_category);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN expand text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN max_payout_referral_for_a_day integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN referral_program_start_date timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.payout_config ADD COLUMN coin_redemption_minimum_limit double precision ;