ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN distance double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN distance double precision NOT NULL;

CREATE TABLE atlas_driver_offer_bpp.booking_location (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   lat DOUBLE PRECISION NOT NULL,
   lon DOUBLE PRECISION NOT NULL,
   city CHARACTER VARYING(255),
   state CHARACTER VARYING(255),
   country CHARACTER VARYING(255),
   street CHARACTER VARYING(255),
   door CHARACTER VARYING(255),
   building CHARACTER VARYING(255),
   area_code CHARACTER VARYING(255),
   area CHARACTER VARYING(255),
   created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.ride_booking (
id character(36) NOT NULL PRIMARY KEY,
status character varying(255) NOT NULL,
provider_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.organization(id),
bap_id character varying(255) NOT NULL,
bap_uri character varying(255) NOT NULL,
from_location_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.booking_location(id),
to_location_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.booking_location(id),
vehicle_variant character varying(255) NOT NULL,
estimated_fare double precision NOT NULL,
estimated_distance double precision NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.ride_booking OWNER TO atlas_driver_offer_bpp_user;
