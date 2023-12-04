DROP TABLE IF EXISTS atlas_app.ticket_booking_service;

CREATE TABLE atlas_app.ticket_booking_service ();

ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN amount Text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN btype Text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN created_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN expiry_date timestamp with time zone ;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN status Text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN ticket_service_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN updated_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD COLUMN verification_count integer NOT NULL;
ALTER TABLE atlas_app.ticket_booking_service ADD PRIMARY KEY ( id);