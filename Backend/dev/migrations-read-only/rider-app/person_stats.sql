CREATE TABLE atlas_app.person_stats ();

ALTER TABLE atlas_app.person_stats ADD COLUMN completed_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN driver_cancelled_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN evening_peak_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN morning_peak_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN off_peak_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_stats ADD COLUMN user_cancelled_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN weekday_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN weekend_peak_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN weekend_rides integer NOT NULL;
ALTER TABLE atlas_app.person_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_stats ADD PRIMARY KEY ( person_id);


------- SQL updates -------

ALTER TABLE atlas_app.person_stats ADD COLUMN referral_count integer NOT NULL default 0;


------- SQL updates -------

ALTER TABLE atlas_app.person_stats ALTER COLUMN created_at DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.person_stats ADD COLUMN tickets_booked_in_event integer  default 0;


------- SQL updates -------

ALTER TABLE atlas_app.person_stats ADD COLUMN valid_activations integer ;
ALTER TABLE atlas_app.person_stats ADD COLUMN referred_by_earnings double precision ;
ALTER TABLE atlas_app.person_stats ADD COLUMN referral_earnings double precision ;
ALTER TABLE atlas_app.person_stats ADD COLUMN referral_amount_paid double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.person_stats ADD COLUMN referred_by_earnings_payout_status text ;
ALTER TABLE atlas_app.person_stats ADD COLUMN backlog_payout_status text ;
ALTER TABLE atlas_app.person_stats ADD COLUMN backlog_payout_amount double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.person_stats ADD COLUMN is_backfilled boolean ;