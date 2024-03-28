CREATE TABLE atlas_app.ride ();

ALTER TABLE atlas_app.ride ADD COLUMN allowed_edit_location_attempts int ;
ALTER TABLE atlas_app.ride ADD COLUMN booking_id character(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN bpp_ride_id character(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN chargeable_distance numeric(30,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN client_id character varying(36) ;
ALTER TABLE atlas_app.ride ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ride ADD COLUMN driver_arrival_time timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_image text ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_mobile_country_code text ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_mobile_number character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN driver_name character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN driver_rating numeric(10,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN driver_registered_at timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN end_odometer_reading double precision ;
ALTER TABLE atlas_app.ride ADD COLUMN end_otp text ;
ALTER TABLE atlas_app.ride ADD COLUMN fare numeric(30,2) ;

ALTER TABLE atlas_app.ride ADD COLUMN id character(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN is_free_ride boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN merchant_id character(36) ;
ALTER TABLE atlas_app.ride ADD COLUMN merchant_operating_city_id character(36) ;
ALTER TABLE atlas_app.ride ADD COLUMN otp character(4) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN ride_end_time timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN ride_rating bigint ;
ALTER TABLE atlas_app.ride ADD COLUMN ride_start_time timestamp with time zone ;
ALTER TABLE atlas_app.ride ADD COLUMN safety_check_status boolean ;
ALTER TABLE atlas_app.ride ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN start_odometer_reading double precision ;
ALTER TABLE atlas_app.ride ADD COLUMN status character varying(255) NOT NULL;

ALTER TABLE atlas_app.ride ADD COLUMN total_fare numeric(30,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN tracking_url character varying(255) ;
ALTER TABLE atlas_app.ride ADD COLUMN traveled_distance numeric(30,2) ;
ALTER TABLE atlas_app.ride ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_color character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_model character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_number character varying(255) NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN vehicle_variant character varying(60) NOT NULL;
ALTER TABLE atlas_app.ride ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.ride ALTER COLUMN vehicle_color DROP NOT NULL;
ALTER TABLE atlas_app.ride ADD COLUMN currency character varying(255) ;
