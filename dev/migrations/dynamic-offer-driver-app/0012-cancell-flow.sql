CREATE TABLE atlas_driver_offer_bpp.cancellation_reason (
  reason_code character varying(255) NOT NULL,
  description character varying(255) NOT NULL,
  enabled boolean NOT NULL,
  CONSTRAINT cancellation_reason_pkey PRIMARY KEY (reason_code)
);

CREATE TABLE atlas_driver_offer_bpp.booking_cancellation_reason (
  id character(36) PRIMARY KEY NOT NULL,
  driver_id character(36) REFERENCES atlas_driver_offer_bpp.person(id),
  booking_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.ride_booking(id),
  ride_id character(36) REFERENCES atlas_driver_offer_bpp.ride(id),
  source character varying(255) NOT NULL,
  reason_code character varying(255),
  additional_info character varying(255)
);