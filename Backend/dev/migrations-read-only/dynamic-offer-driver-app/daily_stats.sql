CREATE TABLE atlas_driver_offer_bpp.daily_stats ();

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN merchant_local_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN num_rides integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN total_distance integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN total_earnings integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD PRIMARY KEY ( id, driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats DROP CONSTRAINT daily_stats_pkey;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN total_earnings_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN currency character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN distance_unit character varying(255) ;




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN referral_earnings double precision ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN referral_counts integer ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN payout_status text ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN payout_order_status text ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN payout_order_id text ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN activated_valid_rides integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN toll_charges double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN bounus_earnings double precision ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN tip_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN cancellation_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN total_ride_time integer;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN bonus_earnings double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN merchant_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN num_fleets_onboarded integer ;
ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN num_drivers_onboarded integer ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN online_duration integer ;




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.daily_stats ADD COLUMN commission_charges double precision ;