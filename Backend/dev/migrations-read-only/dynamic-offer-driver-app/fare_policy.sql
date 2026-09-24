-- No need to run this migration, these are already there !! --
CREATE TABLE atlas_driver_offer_bpp.fare_policy ();


ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN airport_convenience_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN max_allowed_trip_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN min_allowed_trip_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN booth_charges text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN business_discount_percentage double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN cancellation_commission_charge_config text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN cancellation_fare_policy_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN card_charge_per_distance_unit_multiplier double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN fixed_card_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN cgst double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN commission_charge_config text ;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN congestion_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN driver_allowance double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN driver_cancellation_not_allowed boolean ;

ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN fare_policy_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN govt_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN night_shift_end time without time zone ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN night_shift_start time without time zone ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN parking_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN per_distance_unit_insurance_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN per_luggage_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN per_minute_ride_extra_time_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN per_stop_charge double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN personal_discount_percentage double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN pet_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN pickup_buffer_in_secs_for_night_shift_cal integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN platform_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN platform_fee_charges_by text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN priority_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN return_fee text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN ride_extra_time_charge_grace_period integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN scheduling_charge text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN service_charge integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN service_charge_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN sgst double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN tip_options integer ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN toll_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN toll_tax_charge_config text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN vat_charge_config text ;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD PRIMARY KEY ( id);

-- No need to run migrations till here, these are already there !! --