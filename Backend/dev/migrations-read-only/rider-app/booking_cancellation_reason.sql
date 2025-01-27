CREATE TABLE atlas_app.booking_cancellation_reason ();

ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN additional_info text ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN created_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_cancellation_location_lat double precision ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_cancellation_location_lon double precision ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN distance_unit character varying(255) ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_dist_to_pickup bigint ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN driver_dist_to_pickup_value double precision ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN reason_code character varying(255) ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN reason_stage character varying(255) ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN ride_id character varying(36) ;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN source character varying(255) NOT NULL;
ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN updated_at timestamp with time zone  default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.booking_cancellation_reason ADD PRIMARY KEY ( booking_id);


------- SQL updates -------

ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN rider_id character varying(36) ;