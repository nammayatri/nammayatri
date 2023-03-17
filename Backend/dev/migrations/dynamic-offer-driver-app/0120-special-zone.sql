CREATE TABLE atlas_driver_offer_bpp.search_request_special_zone (
id character(36) NOT NULL,
transaction_id character(36) NOT NULL,
message_id character(36) NOT NULL,
start_time timestamp with time zone NOT NULL,
valid_till timestamp with time zone NOT NULL,
provider_id character varying(255) NOT NULL,
from_location_id character varying(36),
to_location_id character varying(36),
bap_id character varying(255) NOT NULL,
bap_uri character varying(255) NOT NULL,
estimated_duration integer,
estimated_distance integer,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
CONSTRAINT  idx_search_request_special_zone_primary PRIMARY KEY (id)
);

ALTER TABLE atlas_driver_offer_bpp.search_request_special_zone OWNER TO atlas_driver_offer_bpp_user;


CREATE TABLE atlas_driver_offer_bpp.quote_special_zone (
id character(36) NOT NULL PRIMARY KEY,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request_special_zone (id) NOT NULL,
provider_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id) NOT NULL,

distance integer NOT NULL,
estimated_fare double precision NOT NULL,
fare_parameters_id character(36) REFERENCES atlas_driver_offer_bpp.fare_parameters(id) NOT NULL,
estimated_finish_time timestamp with time zone NOT NULL,
vehicle_variant character varying(255) NOT NULL,
valid_till timestamp NOT NULL,
created_at timestamp NOT NULL,
updated_at timestamp NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.quote_special_zone OWNER TO atlas_driver_offer_bpp_user;
