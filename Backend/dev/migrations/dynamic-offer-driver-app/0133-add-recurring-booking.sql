CREATE TABLE atlas_driver_offer_bpp.recurring_booking (
  id character varying(255) PRIMARY KEY,
  scheduled_days jsonb not null,
  start_date date not null,
  end_date date,
  pickup_time time not null,
  status character varying(255) not null,
  provider_id character varying(255) not null,
  bap_id character varying(255) not null,
  bap_uri character varying(255) not null,
  fare_policy_id character varying(255),
  rider_id character varying(255),
  from_location_id character varying(255),
  to_location_id character varying(255)
);

CREATE TABLE atlas_driver_offer_bpp.timetable (
  id character varying(255) PRIMARY KEY,
  recurring_booking_id character varying(255) not null,
  booking_id character varying(255),
  pickup_date date not null,
  pickup_time time not null,
  status character varying(255) not null
);

ALTER TABLE atlas_driver_offer_bpp.timetable OWNER TO atlas_driver_offer_bpp_user;

ALTER TABLE atlas_driver_offer_bpp.timetable
	ADD CONSTRAINT timetable_unique_booking_date UNIQUE (recurring_booking_id, pickup_date);

ALTER TABLE atlas_driver_offer_bpp.recurring_booking OWNER TO atlas_driver_offer_bpp_user;

ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN automated_search boolean DEFAULT false;
