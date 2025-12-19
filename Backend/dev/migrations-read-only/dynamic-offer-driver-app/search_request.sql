CREATE TABLE atlas_driver_offer_bpp.search_request ();

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN area text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN auto_assign_enabled boolean  default false;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN bap_city text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN bap_country text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN bap_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN bap_uri character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN customer_cancellation_dues double precision  default 0;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN customer_language character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN device text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN disability_tag text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN driver_default_extra_fee double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN driver_default_extra_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN estimated_distance integer ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN estimated_duration integer ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN from_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_blocked_route boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_customer_preffered_search_route boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_reallocation_enabled boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_scheduled boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN message_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN pickup_zone_gate_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN provider_id character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN rider_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN special_location_tag text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN start_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN to_location_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN toll_charges double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN toll_names text[] ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN valid_till timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN round_trip boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN return_time timestamp with time zone ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_advance_booking_enabled boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_dashboard_request boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN customer_namma_tags text[] ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN search_tags text[] ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN trip_category text ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN to_loc_geohash text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN from_loc_geohash text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN pooling_logic_version integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN has_stops boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN pooling_config_version integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN driver_id_for_search character varying(36) ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN dynamic_pricing_logic_version integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN config_in_experiment_versions json ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN parcel_type text ;
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN parcel_quantity integer ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN prefer_safety_plus boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_reserve_ride boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN is_multimodal_search boolean ;

------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN number_of_luggages integer ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN payment_mode text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN toll_ids text[] ;
