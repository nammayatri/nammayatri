CREATE TABLE atlas_driver_offer_bpp.rider_details ();

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN cancellation_dues double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN dispute_chances_used integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN has_taken_valid_ride boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN has_taken_valid_ride_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN mobile_country_code character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN mobile_number_encrypted character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN mobile_number_hash bytea NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN night_safety_checks boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN otp_code text ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referral_code character varying(15) ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referred_at timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN referred_by_driver character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN payout_flag_reason text ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN first_ride_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ALTER COLUMN first_ride_id TYPE text;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN is_device_id_exists boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN is_flag_confirmed boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN merchant_operating_city_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN bap_id character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN valid_cancellations integer ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN total_bookings integer ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN completed_rides integer ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN cancelled_rides integer ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN cancellation_due_rides integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN waived_off_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN no_of_times_waive_off_used integer ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN no_of_times_canellation_dues_paid integer ;
ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN cancellation_dues_paid double precision ;