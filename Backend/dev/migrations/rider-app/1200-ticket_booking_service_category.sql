CREATE TABLE atlas_app.ticket_booking_service_category ();

ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN amount Text ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN booked_seats integer ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN name text ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD COLUMN ticket_booking_service_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_service_category ADD PRIMARY KEY ( id);