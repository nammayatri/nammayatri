CREATE TABLE atlas_driver_offer_bpp.quote_revised_special_zone (
id character(36) NOT NULL PRIMARY KEY,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request (id) NOT NULL,
provider_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id) NOT NULL,
distance integer NOT NULL,
estimated_fare double precision NOT NULL,
fare_parameters_id character(36) REFERENCES atlas_driver_offer_bpp.fare_parameters(id) NOT NULL,
estimated_finish_time timestamp with time zone NOT NULL,
vehicle_variant character varying(255) NOT NULL,
valid_till TIMESTAMP WITH TIME ZONE NOT NULL,
created_at TIMESTAMP WITH TIME ZONE NOT NULL,
updated_at TIMESTAMP WITH TIME ZONE NOT NULL,
special_location_tag text,
trip_category text,
fare_policy_id character varying(36),
is_scheduled boolean
);
ALTER TABLE atlas_driver_offer_bpp.quote_revised_special_zone OWNER TO atlas_driver_offer_bpp_user;
