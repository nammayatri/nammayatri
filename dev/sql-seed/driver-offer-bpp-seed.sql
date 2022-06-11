-- auxiliary settings
CREATE USER atlas_driver_offer_bpp_user WITH PASSWORD 'atlas';
SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;
CREATE SCHEMA atlas_driver_offer_bpp;
ALTER SCHEMA atlas_driver_offer_bpp OWNER TO atlas_driver_offer_bpp_user;

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

CREATE TABLE atlas_driver_offer_bpp.person (
id character(36) NOT NULL,
first_name character varying(255),
middle_name character varying(255),
last_name character varying(255),
full_name character varying(255),
role character varying(255) NOT NULL,
gender character varying(255) NOT NULL,
identifier_type character varying(255) NOT NULL,
email character varying(255),
password_hash bytea,
mobile_number_encrypted character varying(255),
mobile_number_hash bytea,
mobile_country_code character varying(255),
identifier character varying(255),
is_new boolean NOT NULL,
udf1 character varying(255),
udf2 character varying(255),
organization_id character varying(255),
device_token character varying(255),
description character varying(255),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
rating double precision
,CONSTRAINT  idx_16419_primary PRIMARY KEY (id)
,CONSTRAINT  person_unique_mobile_number_country_code UNIQUE (mobile_country_code, mobile_number_hash)
,CONSTRAINT  unique_email UNIQUE (email)
,CONSTRAINT  unique_identifier UNIQUE (identifier)
);
ALTER TABLE atlas_driver_offer_bpp.person OWNER TO atlas_driver_offer_bpp_user;
CREATE INDEX idx_16419_organization_id ON atlas_driver_offer_bpp.person USING btree (organization_id);
CREATE INDEX idx_16419_role ON atlas_driver_offer_bpp.person USING btree (role);

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

CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver (
id character(36) NOT NULL PRIMARY KEY,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request (id) NOT NULL,
distance_to_pickup bigint NOT NULL,
duration_to_pickup bigint NOT NULL,
vehicle_variant character varying(255) NOT NULL,
base_fare double precision NOT NULL,
search_request_valid_till timestamp NOT NULL,
driver_id character(36) REFERENCES atlas_driver_offer_bpp.person (id) NOT NULL,
created_at timestamp NOT NULL,
unique (search_request_id, driver_id)
);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.driver_quote (
id character(36) NOT NULL PRIMARY KEY,
status character varying(255) NOT NULL,
search_request_id character(36) REFERENCES atlas_driver_offer_bpp.search_request (id) NOT NULL,
driver_id character(36) REFERENCES atlas_driver_offer_bpp.person (id) NOT NULL,
base_fare double precision NOT NULL,
extra_fare_selected double precision,
distance_to_pickup bigint NOT NULL,
duration_to_pickup bigint NOT NULL,
vehicle_variant character varying(255) NOT NULL,
valid_till timestamp NOT NULL,
created_at timestamp NOT NULL,
updated_at timestamp NOT NULL
);
ALTER TABLE atlas_driver_offer_bpp.driver_quote OWNER TO atlas_driver_offer_bpp_user;

CREATE TABLE atlas_driver_offer_bpp.registration_token (
id character(36) NOT NULL,
auth_medium character varying(255) NOT NULL,
auth_type character varying(255) NOT NULL,
auth_value_hash character varying(1024) NOT NULL,
token character varying(1024) NOT NULL,
verified boolean NOT NULL,
auth_expiry bigint NOT NULL,
token_expiry bigint NOT NULL,
attempts bigint NOT NULL,
entity_id character(36) NOT NULL,
entity_type character(36) NOT NULL,
info text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16435_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_driver_offer_bpp.registration_token OWNER TO atlas_driver_offer_bpp_user;
CREATE INDEX idx_16435_entity_id ON atlas_driver_offer_bpp.registration_token USING btree (entity_id);
CREATE INDEX idx_16435_entity_type ON atlas_driver_offer_bpp.registration_token USING btree (entity_type);

CREATE TABLE atlas_driver_offer_bpp.driver_information (
driver_id character(36) NOT NULL,
active boolean DEFAULT false NOT NULL,
on_ride boolean DEFAULT false NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
enabled boolean NOT NULL
,CONSTRAINT  driver_information_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_information_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);
ALTER TABLE atlas_driver_offer_bpp.driver_information OWNER TO atlas_driver_offer_bpp_user;

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

CREATE TABLE atlas_driver_offer_bpp.vehicle (
id character(36) NOT NULL,
capacity bigint,
category character varying(255),
make character varying(255),
model character varying(255) NOT NULL,
size character varying(255),
variant character varying(255) NOT NULL,
color character varying(255) NOT NULL,
energy_type character varying(255),
registration_no character varying(255) NOT NULL,
registration_category character varying(255),
organization_id character(36),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16451_primary PRIMARY KEY (id)
,CONSTRAINT  unique_registration_no UNIQUE (registration_no)
);
ALTER TABLE atlas_driver_offer_bpp.vehicle OWNER TO atlas_driver_offer_bpp_user;
CREATE INDEX idx_16451_organization_id ON atlas_driver_offer_bpp.vehicle USING btree (organization_id);


