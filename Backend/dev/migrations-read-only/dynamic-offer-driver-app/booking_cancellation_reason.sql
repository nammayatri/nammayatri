CREATE TABLE atlas_driver_offer_bpp.booking_cancellation_reason ();

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN additional_info text ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN driver_cancellation_location_lat double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN driver_cancellation_location_lon double precision ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN driver_dist_to_pickup integer ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN driver_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN reason_code text ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN source text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD PRIMARY KEY ( booking_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN distance_unit character varying(255) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN merchant_operating_city_id character varying(36) ;