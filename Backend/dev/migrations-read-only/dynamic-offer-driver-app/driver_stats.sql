CREATE TABLE atlas_driver_offer_bpp.driver_stats ();

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN bonus_earned_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN coin_coverted_to_cash_left double precision  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN earnings_missed_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN idle_since timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN late_night_trips integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN rides_cancelled integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_coins_converted_cash double precision  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_distance double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings double precision NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_earnings_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_rides integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_rides_assigned integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD PRIMARY KEY ( driver_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN fav_rider_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_ratings integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_rating_score integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN rating text ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN is_valid_rating boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats DROP COLUMN rating;




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_valid_activated_rides integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_referral_counts integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_payout_earnings double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_payout_amount_paid double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN valid_driver_cancellation_tag_count integer  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN valid_customer_cancellation_tag_count integer  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN valid_cancellation_tags_stats_start_date timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN num_fleets_onboarded integer  default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN num_drivers_onboarded integer  default 0;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN safety_plus_ride_count integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN safety_plus_earnings double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN online_duration integer  default 0;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN blacklist_coin_events text[]  default '{}';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN total_request_count integer ;
ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN acceptation_request_count integer ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ADD COLUMN coins_converted_to_direct_payout_cash double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.driver_stats ALTER COLUMN coins_converted_to_direct_payout_cash SET DEFAULT 0;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

