CREATE TABLE atlas_driver_offer_bpp.fleet_booking_information ();

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN booked_seats integer ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN booking_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN fleet_owner_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN person_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN place_name text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN service_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN service_name text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN status text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN vehicle_no text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN visit_date date ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN ticket_place_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN ticket_booking_short_id text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN ticket_booking_service_short_id text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN payment_method text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN customer_name text ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN customer_mobile_number_hash bytea ;
ALTER TABLE atlas_driver_offer_bpp.fleet_booking_information ADD COLUMN customer_mobile_number_encrypted character varying(255) ;