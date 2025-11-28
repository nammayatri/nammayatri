CREATE TABLE atlas_driver_offer_bpp.booking ();

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN area text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN bap_city text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN bap_country text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN bap_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN bap_uri character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN disability_tag text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN distance_to_pickup double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_distance double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_fare double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN fare_parameters_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN from_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_scheduled boolean ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN max_estimated_distance double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN payment_method_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN payment_url text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN primary_exophone character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN provider_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN rider_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN rider_name character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN special_zone_otp_code character(4) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN status character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN stop_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN to_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN transaction_id character(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN booking_type character(36) NOT NULL default 'NormalBooking';
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN vehicle_variant character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN vehicle_service_tier_air_conditioned double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN vehicle_service_tier_name text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN vehicle_service_tier_seating_capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN toll_names text[] ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN currency character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimate_id character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN round_trip boolean ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN return_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN payment_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_air_conditioned boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_dashboard_request boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN sender_name text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN sender_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN receiver_name text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN receiver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN initiated_as text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN sender_primary_exophone text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN receiver_primary_exophone text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN from_loc_geohash text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN to_loc_geohash text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN has_stops boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_referred_ride boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN dynamic_pricing_logic_version integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimated_congestion_charge double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN config_in_experiment_versions json ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN parcel_type text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN parcel_quantity integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_safety_plus boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN coins_rewarded_on_gold_tier_ride integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_insured boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN insured_amount text ;
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN is_pet_ride boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN exotel_declined_call_status_receiving_time timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN number_of_luggages integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN billing_category text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN payment_instrument text ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN commission double precision ;