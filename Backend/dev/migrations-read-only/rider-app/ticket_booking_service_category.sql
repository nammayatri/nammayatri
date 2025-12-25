CREATE TABLE atlas_app.ticket_booking_service_category ();

ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN booked_seats integer NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN service_category_id text ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN ticket_booking_service_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_service_category ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service_category ALTER COLUMN booked_seats SET DEFAULT 0;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN visit_date date ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN event_cancelled_by text ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN cancelled_seats integer ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN btype text ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN amount_to_refund double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN vendor_split_details json ;