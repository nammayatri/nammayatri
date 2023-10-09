CREATE TABLE atlas_driver_offer_bpp.search_request_rental (
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
base_duration integer,
base_distance integer,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
CONSTRAINT  idx_search_request_rental PRIMARY KEY (id)
);

ALTER TABLE atlas_driver_offer_bpp.search_request_rental OWNER TO atlas_driver_offer_bpp_user;


CREATE TABLE atlas_driver_offer_bpp.quote_rental (
id character(36) NOT NULL PRIMARY KEY,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request_special_zone (id) NOT NULL,
provider_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id) NOT NULL,
base_distance integer NOT NULL,
base_duration integer NOT NULL,
base_estimated_fare double precision NOT NULL,
fare_parameters_id character(36) REFERENCES atlas_driver_offer_bpp.fare_parameters(id) NOT NULL,
estimated_finish_time timestamp with time zone NOT NULL,
vehicle_variant character varying(255) NOT NULL,
valid_till timestamp NOT NULL,
created_at timestamp NOT NULL,
updated_at timestamp NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.quote_rental OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.rental_fare_slab (
    id character(36) NOT NULL PRIMARY KEY,
    provider_id character(36) REFERENCES atlas_driver_offer_bpp.merchant (id) NOT NULL,
    base_distance integer NOT NULL,
    base_duration integer NOT NULL,
    extra_km_fare integer NOT NULL,
    extra_hours_fare integer NOT NULL,
    platform_fee_charge JSON,
    platform_fee_cgst JSON,
    platform_fee_sgst JSON,
    waiting_charge JSON,
    free_waiting_time JSON,
    night_shift_charge JSON,
)
