CREATE TABLE atlas_app.ticket_booking_service ();

ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN btype text NOT NULL default 'Duration 03:30:00 12:00:00';
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN expiry_date timestamp with time zone ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN ticket_service_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN verification_count integer NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_service ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service ALTER COLUMN merchant_operating_city_id SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN visit_date date ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN cancelled_seats integer ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN booked_seats integer NOT NULL default 0;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN b_hour_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN vendor_split_details json ;