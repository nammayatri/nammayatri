
CREATE TABLE atlas_driver_offer_bpp.organization (
id character(36) NOT NULL,
name character varying(255),
short_id character varying(255) NOT NULL,
gstin character varying(255),
status character varying(255),
type character varying(255),
domain character varying(255),
verified boolean NOT NULL,
enabled boolean DEFAULT true NOT NULL,
description text,
mobile_number text,
mobile_country_code character varying(255),
from_time timestamp with time zone,
to_time timestamp with time zone,
api_key text,
head_count bigint,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
info text,
unique_key_id character varying(255) DEFAULT 'FIXME'::character varying NOT NULL
,CONSTRAINT  idx_16410_primary PRIMARY KEY (id)
,CONSTRAINT  organization_unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number)
,CONSTRAINT  unique_api_key UNIQUE (api_key)
,CONSTRAINT  unique_short_id UNIQUE (short_id)
);
ALTER TABLE atlas_driver_offer_bpp.organization OWNER TO atlas_driver_offer_bpp_user;
CREATE INDEX idx_organization_short_id ON atlas_driver_offer_bpp.organization USING btree (short_id);

CREATE TABLE atlas_driver_offer_bpp.search_request_location (
id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
point public.geography(Point,4326) NOT NULL,
city character varying(255),
state character varying(255),
country character varying(255),
street character varying(255),
door character varying(255),
building character varying(255),
area_code character varying(255),
area character varying(255),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16402_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_driver_offer_bpp.search_request_location OWNER TO atlas_driver_offer_bpp_user;
CREATE INDEX idx_16402_city ON atlas_driver_offer_bpp.search_request_location USING btree (city);
CREATE INDEX idx_16402_state ON atlas_driver_offer_bpp.search_request_location USING btree (state);



CREATE TABLE atlas_driver_offer_bpp.search_request (
id character(36) NOT NULL,
transaction_id character(36) NOT NULL,
message_id character(36) NOT NULL,
valid_till timestamp with time zone NOT NULL,
provider_id character varying(255) NOT NULL,
from_location_id character varying(36),
to_location_id character varying(36),
bap_id character varying(255) NOT NULL,
bap_uri character varying(255) NOT NULL,
gateway_uri character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16386_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_driver_offer_bpp.search_request OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.driver_location (
driver_id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
point public.geography(Point,4326) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  driver_location_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_location_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);
ALTER TABLE atlas_driver_offer_bpp.driver_location OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.driver_stats (
driver_id character(36) NOT NULL,
idle_since timestamp with time zone
,CONSTRAINT  driver_stats_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_stats_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);
ALTER TABLE atlas_driver_offer_bpp.driver_stats OWNER TO atlas_driver_offer_bpp_user;

-- fare policy
CREATE TABLE atlas_driver_offer_bpp.fare_policy (
id character(36) NOT NULL,
organization_id character (36) NOT NULL,
base_fare double precision,
night_shift_start time without time zone,
night_shift_end time without time zone,
night_shift_rate double precision,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  fare_policy_org_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_driver_offer_bpp.organization(id)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.fare_policy_per_extra_km_rate (
id character(36) NOT NULL,
organization_id character(36) NOT NULL,
distance_range_start double precision NOT NULL,
fare double precision NOT NULL
,CONSTRAINT  fare_policy_extra_km_rate_unique_extra_distance_range_start UNIQUE (organization_id, distance_range_start)
,CONSTRAINT  fare_policy_per_extra_km_rate_pkey PRIMARY KEY (id)
,CONSTRAINT  fare_policy_per_extra_km_rate_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_driver_offer_bpp.organization(id)
);
ALTER TABLE atlas_driver_offer_bpp.fare_policy_per_extra_km_rate OWNER TO atlas_driver_offer_bpp_user;


