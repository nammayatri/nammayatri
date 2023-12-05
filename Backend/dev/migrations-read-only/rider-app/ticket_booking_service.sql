CREATE TABLE ticket_booking_service ();

ALTER TABLE ticket_booking_service ADD COLUMN amount NO_SQL_TYPE NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN btype NO_SQL_TYPE NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN created_at timestamp with time zone NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN expiry_date timestamp with time zone ;
ALTER TABLE ticket_booking_service ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN short_id character varying(36) NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN status NO_SQL_TYPE NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN ticket_service_id character varying(36) NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN updated_at timestamp with time zone NOT NULL;
ALTER TABLE ticket_booking_service ADD COLUMN verification_count integer NOT NULL;
ALTER TABLE ticket_booking_service ADD PRIMARY KEY ( id);