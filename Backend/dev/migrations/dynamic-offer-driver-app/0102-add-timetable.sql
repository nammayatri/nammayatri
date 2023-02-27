CREATE TABLE atlas_driver_offer_bpp.timetable (
  id character varying(255) PRIMARY KEY,
  recurring_booking_id character varying(255) not null,
  pickup_date date not null,
  pickup_time time not null,
  status character varying(255) not null
);

ALTER TABLE atlas_driver_offer_bpp.timetable OWNER TO atlas_driver_offer_bpp_user;

ALTER TABLE atlas_driver_offer_bpp.timetable
	ADD CONSTRAINT timetable_unique_booking_date UNIQUE (recurring_booking_id, pickup_date);
