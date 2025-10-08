CREATE TABLE atlas_driver_offer_bpp.vehicle_service_tier ();

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN air_conditioned double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN allowed_vehicle_variant text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN driver_rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN long_description text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN luggage_capacity integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN seating_capacity integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN short_description text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN vehicle_rating double precision ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN service_tier_type text NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ALTER COLUMN seating_capacity DROP NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN default_for_vehicle_variant text[] NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN priority integer NOT NULL default 0;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN auto_selected_vehicle_variant text[] NOT NULL default '{}';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN ventilator integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN oxygen double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN is_air_conditioned boolean ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN air_conditioned_threshold double precision ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN is_rentals_enabled boolean ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN is_intercity_enabled boolean ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN vehicle_icon_url character varying(255) ;


------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN vehicle_category text ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN fare_addition_per_km_over_base_service_tier integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN base_vehicle_service_tier boolean ;



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN stop_fcm_threshold integer ;
ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN stop_fcm_suppress_count integer ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.vehicle_service_tier ADD COLUMN schedule_booking_list_eligibility_tags text[] ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

