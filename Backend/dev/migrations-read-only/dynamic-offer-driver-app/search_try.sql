CREATE TABLE atlas_driver_offer_bpp.search_try ();

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN base_fare integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN base_fare_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN currency character varying(255) ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN customer_extra_fee integer ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN customer_extra_fee_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN estimate_id character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN estimate_ids text[] ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN is_scheduled boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN message_id character (36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN search_repeat_counter integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN search_repeat_type character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN trip_category text ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_variant character varying (255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_service_tier_name text ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN is_advanced_booking_enabled boolean ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN service_tier_array text[] ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN prefer_safety_plus boolean ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN vehicle_category text ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN pet_charges_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN pet_charges integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN billing_category text ;