CREATE TABLE ticket_booking_service_category ();

ALTER TABLE ticket_booking_service_category ADD COLUMN amount NO_SQL_TYPE ;
ALTER TABLE ticket_booking_service_category ADD COLUMN booked_seats integer ;
ALTER TABLE ticket_booking_service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE ticket_booking_service_category ADD COLUMN name text ;
ALTER TABLE ticket_booking_service_category ADD COLUMN ticket_booking_service_id character varying(36) ;
ALTER TABLE ticket_booking_service_category ADD PRIMARY KEY ( id);