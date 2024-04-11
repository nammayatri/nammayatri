CREATE TABLE atlas_app.estimate ();

ALTER TABLE atlas_app.estimate ADD COLUMN bpp_estimate_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.estimate ADD COLUMN device text ;
ALTER TABLE atlas_app.estimate ADD COLUMN discount text ;
ALTER TABLE atlas_app.estimate ADD COLUMN drivers_location text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN estimate_breakup_list text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_fare text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_total_fare text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN item_id text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.estimate ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_info text ;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_completed_rides_count integer NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_id text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_mobile_number text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_name text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN provider_url text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN service_tier_name text ;
ALTER TABLE atlas_app.estimate ADD COLUMN service_tier_short_desc text ;
ALTER TABLE atlas_app.estimate ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_app.estimate ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN total_fare_range_max_fare text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN total_fare_range_min_fare text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN trip_terms text ;
ALTER TABLE atlas_app.estimate ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.estimate ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_service_tier_type text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charges_waiting_charge_per_min text ;
ALTER TABLE atlas_app.estimate ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN min_fare double precision NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN max_fare double precision NOT NULL;
ALTER TABLE atlas_app.estimate DROP COLUMN total_fare_range_min_fare;
ALTER TABLE atlas_app.estimate DROP COLUMN total_fare_range_max_fare;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN trip_terms_id text ;
ALTER TABLE atlas_app.estimate ADD COLUMN old_night_shift_charge text ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_start time without time zone ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_end time without time zone ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge_amount double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge integer ;
ALTER TABLE atlas_app.estimate ADD COLUMN min_total_fare double precision NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN max_total_fare double precision NOT NULL;
ALTER TABLE atlas_app.estimate DROP COLUMN vehicle_service_tier_type;
ALTER TABLE atlas_app.estimate DROP COLUMN trip_terms;
ALTER TABLE atlas_app.estimate DROP COLUMN night_shift_info;
ALTER TABLE atlas_app.estimate DROP COLUMN min_fare;
ALTER TABLE atlas_app.estimate DROP COLUMN max_fare;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_info text ;
ALTER TABLE atlas_app.estimate DROP COLUMN old_night_shift_charge;
ALTER TABLE atlas_app.estimate DROP COLUMN night_shift_start;
ALTER TABLE atlas_app.estimate DROP COLUMN night_shift_end;
ALTER TABLE atlas_app.estimate DROP COLUMN night_shift_charge_amount;
ALTER TABLE atlas_app.estimate DROP COLUMN night_shift_charge;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN trip_terms text ;
ALTER TABLE atlas_app.estimate DROP COLUMN trip_terms_id;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN trip_terms_id text ;
ALTER TABLE atlas_app.estimate ADD COLUMN old_night_shift_charge text ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_start time without time zone ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_end time without time zone ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge_amount double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge integer ;
ALTER TABLE atlas_app.estimate DROP COLUMN trip_terms;
ALTER TABLE atlas_app.estimate DROP COLUMN night_shift_info;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charge_per_min_amount double precision ;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charge_per_min integer ;
ALTER TABLE atlas_app.estimate DROP COLUMN waiting_charges_waiting_charge_per_min;


------- SQL updates -------

ALTER TABLE atlas_app.estimate DROP COLUMN estimate_breakup_list;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ALTER COLUMN estimated_total_fare TYPE double precision;
ALTER TABLE atlas_app.estimate ALTER COLUMN estimated_fare TYPE double precision;
ALTER TABLE atlas_app.estimate ALTER COLUMN discount TYPE double precision;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_service_tier_type text NOT NULL;
ALTER TABLE atlas_app.estimate DROP COLUMN vehicle_variant;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ADD COLUMN vehicle_variant text NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN estimated_distance_value double precision NOT NULL;
ALTER TABLE atlas_app.estimate ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.estimate DROP COLUMN vehicle_service_tier_type;


------- SQL updates -------

ALTER TABLE atlas_app.estimate ALTER COLUMN estimated_distance_value DROP NOT NULL;