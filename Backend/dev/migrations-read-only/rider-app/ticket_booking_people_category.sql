CREATE TABLE atlas_app.ticket_booking_people_category ();

ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN number_of_units integer NOT NULL;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN price_per_unit double precision NOT NULL;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN ticket_booking_service_category_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_booking_people_category ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN currency text ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN people_category_id character varying(36) ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN number_of_units_cancelled integer ;
ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN amount_to_refund double precision ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_booking_people_category ADD COLUMN vendor_split_details json ;