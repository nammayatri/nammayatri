-- auxiliary settings
CREATE USER atlas_transporter_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_transporter;
ALTER SCHEMA atlas_transporter OWNER TO atlas_transporter_user;
CREATE FUNCTION atlas_transporter.create_fare_policy_on_org_creation() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        INSERT INTO atlas_transporter.fare_policy (id, vehicle_variant, organization_id, base_fare, night_shift_start, night_shift_end, night_shift_rate) VALUES
            (atlas_transporter.uuid_generate_v4(), 'SUV', NEW.id, 120.0, NULL, NULL, 1.0),
            (atlas_transporter.uuid_generate_v4(), 'SEDAN', NEW.id, 120.0, NULL, NULL, 1.0),
            (atlas_transporter.uuid_generate_v4(), 'HATCHBACK', NEW.id, 120.0, NULL, NULL, 1.0);
        INSERT INTO atlas_transporter.fare_policy_per_extra_km_rate VALUES
            (atlas_transporter.uuid_generate_v4(), 'SUV', NEW.id, 5000, 12),
            (atlas_transporter.uuid_generate_v4(), 'SEDAN', NEW.id, 5000, 12),
            (atlas_transporter.uuid_generate_v4(), 'HATCHBACK', NEW.id, 5000, 12);
        RETURN NULL;
    END;
$$;
ALTER FUNCTION atlas_transporter.create_fare_policy_on_org_creation() OWNER TO atlas_transporter_user;
CREATE FUNCTION atlas_transporter.uuid_generate_v4() RETURNS character
    LANGUAGE sql IMMUTABLE
    AS $$SELECT uuid_in(overlay(overlay(md5(random()::text
        || ':' || clock_timestamp()::text) placing '4' from 13)
        placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);$$;
ALTER FUNCTION atlas_transporter.uuid_generate_v4() OWNER TO atlas_transporter_user;

-- tables






CREATE TABLE atlas_transporter.vehicle (
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
ALTER TABLE atlas_transporter.vehicle OWNER TO atlas_transporter_user;
CREATE INDEX idx_16451_organization_id ON atlas_transporter.vehicle USING btree (organization_id);







CREATE TABLE atlas_transporter.cancellation_reason (
reason_code character varying(255) NOT NULL,
description character varying(255) NOT NULL,
enabled boolean NOT NULL
,CONSTRAINT  cancellation_reason_pkey PRIMARY KEY (reason_code)
);
ALTER TABLE atlas_transporter.cancellation_reason OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.location_backup (
id character(36),
location_type character varying(255),
lat double precision,
long double precision,
point public.geography(Point,4326),
ward character varying(255),
district character varying(255),
city character varying(255),
state character varying(255),
country character varying(255),
pincode character varying(255),
address character varying(255),
bound character varying(255),
created_at timestamp with time zone,
updated_at timestamp with time zone
);
ALTER TABLE atlas_transporter.location_backup OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.organization (
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
ALTER TABLE atlas_transporter.organization OWNER TO atlas_transporter_user;
CREATE INDEX idx_organization_short_id ON atlas_transporter.organization USING btree (short_id);
CREATE TRIGGER create_fare_policy_on_org_creation AFTER INSERT ON atlas_transporter.organization FOR EACH ROW EXECUTE FUNCTION atlas_transporter.create_fare_policy_on_org_creation();


CREATE TABLE atlas_transporter.person (
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
ALTER TABLE atlas_transporter.person OWNER TO atlas_transporter_user;
CREATE INDEX idx_16419_organization_id ON atlas_transporter.person USING btree (organization_id);
CREATE INDEX idx_16419_role ON atlas_transporter.person USING btree (role);







CREATE TABLE atlas_transporter.product (
id character(36) NOT NULL,
name character varying(255) NOT NULL,
description character varying(1024),
industry character varying(1024) NOT NULL,
type character varying(255) NOT NULL,
rating character varying(255),
status character varying(255) NOT NULL,
short_id character(36) NOT NULL,
price numeric(30,10) NOT NULL,
review character varying(255),
udf1 character varying(255),
udf2 character varying(255),
udf3 character varying(255),
udf4 character varying(255),
udf5 character varying(255),
info text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16427_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_transporter.product OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.quote (
id character(36) NOT NULL,
request_id character varying(255) NOT NULL,
product_id character varying(255) NOT NULL,
estimated_fare numeric(30,10) NOT NULL,
provider_id character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
traveled_distance double precision DEFAULT 0 NOT NULL,
chargeable_distance double precision,
vehicle_variant character varying(60) NOT NULL,
discount double precision,
estimated_total_fare numeric(30,2) NOT NULL,
total_fare numeric(30,2),
distance_to_nearest_driver double precision NOT NULL,
distance double precision NOT NULL
,CONSTRAINT  idx_16394_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_transporter.quote OWNER TO atlas_transporter_user;
CREATE INDEX idx_16394_case_id ON atlas_transporter.quote USING btree (request_id);
CREATE INDEX idx_16394_organization_id ON atlas_transporter.quote USING btree (provider_id);
CREATE INDEX idx_16394_product_id ON atlas_transporter.quote USING btree (product_id);







CREATE TABLE atlas_transporter.rating (
id character(36) NOT NULL,
ride_id character varying(36) NOT NULL,
rating_value bigint NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
driver_id character(36) NOT NULL
,CONSTRAINT  idx_rating_primary PRIMARY KEY (id)
,CONSTRAINT  rating_product_instance_id_key UNIQUE (ride_id)
);
ALTER TABLE atlas_transporter.rating OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.registration_token (
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
ALTER TABLE atlas_transporter.registration_token OWNER TO atlas_transporter_user;
CREATE INDEX idx_16435_entity_id ON atlas_transporter.registration_token USING btree (entity_id);
CREATE INDEX idx_16435_entity_type ON atlas_transporter.registration_token USING btree (entity_type);







CREATE TABLE atlas_transporter.rider_details (
id character(36) NOT NULL,
mobile_country_code character varying(255) NOT NULL,
mobile_number_encrypted character varying(255) NOT NULL,
mobile_number_hash bytea,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  ride_details_unique_mobile_number UNIQUE (mobile_number_hash, mobile_country_code)
,CONSTRAINT  rider_details_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_transporter.rider_details OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.search_request (
id character(36) NOT NULL,
start_time timestamp with time zone NOT NULL,
valid_till timestamp with time zone NOT NULL,
provider_id character varying(255) NOT NULL,
from_location_id character varying(36),
to_location_id character varying(36),
bap_id character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
transaction_id character(36) NOT NULL,
bap_uri character varying(255) NOT NULL
,CONSTRAINT  idx_16386_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_transporter.search_request OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.search_request_location (
id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
city character varying(255),
state character varying(255),
country character varying(255),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
street character varying(255),
door character varying(255),
building character varying(255),
area_code character varying(255),
area character varying(255)
,CONSTRAINT  idx_16402_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_transporter.search_request_location OWNER TO atlas_transporter_user;
CREATE INDEX idx_16402_city ON atlas_transporter.search_request_location USING btree (city);
CREATE INDEX idx_16402_state ON atlas_transporter.search_request_location USING btree (state);







CREATE TABLE atlas_transporter.transporter_config (
id character(36) NOT NULL,
transporter_id character(36) NOT NULL,
key character(36) NOT NULL,
value character varying(1024) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  transporter_config_pkey PRIMARY KEY (id)
,CONSTRAINT  transporter_config_transporter_id_key_key UNIQUE (transporter_id, key)
);
ALTER TABLE atlas_transporter.transporter_config OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.ride_booking (
id character(36) NOT NULL,
transaction_id character(36) NOT NULL,
request_id character(36) NOT NULL,
quote_id character(36) NOT NULL,
status character varying(255) NOT NULL,
provider_id character(36) NOT NULL,
vehicle_variant character varying(255) NOT NULL,
bap_id character varying(255) NOT NULL,
start_time timestamp with time zone NOT NULL,
rider_id character(36) NOT NULL,
from_location_id character(36) NOT NULL,
to_location_id character(36) NOT NULL,
estimated_fare double precision NOT NULL,
discount double precision,
estimated_total_fare numeric(30,2) NOT NULL,
distance double precision NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
bap_uri character varying(255) NOT NULL
,CONSTRAINT  ride_booking_pkey PRIMARY KEY (id)
,CONSTRAINT  ride_booking_from_location_id_fkey FOREIGN KEY (from_location_id) REFERENCES atlas_transporter.search_request_location(id)
,CONSTRAINT  ride_booking_provider_id_fkey FOREIGN KEY (provider_id) REFERENCES atlas_transporter.organization(id)
,CONSTRAINT  ride_booking_quote_id_fkey FOREIGN KEY (quote_id) REFERENCES atlas_transporter.quote(id)
,CONSTRAINT  ride_booking_request_id_fkey FOREIGN KEY (request_id) REFERENCES atlas_transporter.search_request(id)
,CONSTRAINT  ride_booking_rider_id_fkey FOREIGN KEY (rider_id) REFERENCES atlas_transporter.rider_details(id)
,CONSTRAINT  ride_booking_to_location_id_fkey FOREIGN KEY (to_location_id) REFERENCES atlas_transporter.search_request_location(id)
);
ALTER TABLE atlas_transporter.ride_booking OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.driver_information (
driver_id character(36) NOT NULL,
active boolean DEFAULT false NOT NULL,
on_ride boolean DEFAULT false NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
enabled boolean NOT NULL
,CONSTRAINT  driver_information_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_information_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_transporter.person(id)
);
ALTER TABLE atlas_transporter.driver_information OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.driver_location (
driver_id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
point public.geography(Point,4326) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  driver_location_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_location_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_transporter.person(id)
);
ALTER TABLE atlas_transporter.driver_location OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.driver_stats (
driver_id character(36) NOT NULL,
idle_since timestamp with time zone
,CONSTRAINT  driver_stats_pkey PRIMARY KEY (driver_id)
,CONSTRAINT  driver_stats_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_transporter.person(id)
);
ALTER TABLE atlas_transporter.driver_stats OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.fare_policy (
id character(36) NOT NULL,
vehicle_variant character varying(255) NOT NULL,
organization_id character varying(255) NOT NULL,
base_fare double precision,
night_shift_start time without time zone,
night_shift_end time without time zone,
night_shift_rate double precision,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  fare_policy_org_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_transporter.organization(id)
);
ALTER TABLE atlas_transporter.fare_policy OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.fare_policy_discount (
id character(36) NOT NULL,
vehicle_variant character varying(255) NOT NULL,
organization_id character(36) NOT NULL,
discount double precision NOT NULL,
enabled boolean NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
from_date timestamp with time zone NOT NULL,
to_date timestamp with time zone NOT NULL
,CONSTRAINT  fare_policy_discount_pkey PRIMARY KEY (id)
,CONSTRAINT  fare_policy_discount_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_transporter.organization(id)
);
ALTER TABLE atlas_transporter.fare_policy_discount OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.fare_policy_per_extra_km_rate (
id character(36) NOT NULL,
vehicle_variant character varying(255) NOT NULL,
organization_id character(36) NOT NULL,
distance_range_start double precision NOT NULL,
fare double precision NOT NULL
,CONSTRAINT  fare_policy_extra_km_rate_unique_extra_distance_range_start UNIQUE (vehicle_variant, organization_id, distance_range_start)
,CONSTRAINT  fare_policy_per_extra_km_rate_pkey PRIMARY KEY (id)
,CONSTRAINT  fare_policy_per_extra_km_rate_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_transporter.organization(id)
);
ALTER TABLE atlas_transporter.fare_policy_per_extra_km_rate OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.discount_transaction (
ride_booking_id character(36) NOT NULL,
organization_id character(36) NOT NULL,
discount double precision NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  discount_transaction_pkey PRIMARY KEY (ride_booking_id)
,CONSTRAINT  discount_transaction_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES atlas_transporter.organization(id)
,CONSTRAINT  discount_transaction_ride_booking_id_fkey FOREIGN KEY (ride_booking_id) REFERENCES atlas_transporter.ride_booking(id)
);
ALTER TABLE atlas_transporter.discount_transaction OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.ride_cancellation_reason (
ride_booking_id character(36) NOT NULL,
source character varying(255) NOT NULL,
reason_code character varying(255),
additional_info character varying(255)
,CONSTRAINT  ride_cancellation_reason_pkey PRIMARY KEY (ride_booking_id)
,CONSTRAINT  ride_cancellation_reason_ride_booking_id_fkey FOREIGN KEY (ride_booking_id) REFERENCES atlas_transporter.ride_booking(id)
);
ALTER TABLE atlas_transporter.ride_cancellation_reason OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.ride_request (
id character(36) NOT NULL,
ride_booking_id character(36) NOT NULL,
short_org_id character varying(255) NOT NULL,
created_at timestamp with time zone NOT NULL,
type character varying(20) NOT NULL,
info text
,CONSTRAINT  ride_request_pkey PRIMARY KEY (id)
,CONSTRAINT  ride_request_ride_booking_id_fkey FOREIGN KEY (ride_booking_id) REFERENCES atlas_transporter.ride_booking(id)
);
ALTER TABLE atlas_transporter.ride_request OWNER TO atlas_transporter_user;
CREATE INDEX ride_request_ride_id_idx ON atlas_transporter.ride_request USING btree (ride_booking_id);







CREATE TABLE atlas_transporter.notification_status (
id character(36) NOT NULL,
ride_booking_id character(36) NOT NULL,
driver_id character(36) NOT NULL,
status character varying(20) NOT NULL,
expires_at timestamp with time zone NOT NULL
,CONSTRAINT  notification_status_pkey PRIMARY KEY (id)
,CONSTRAINT  notification_status_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_transporter.person(id)
,CONSTRAINT  notification_status_ride_booking_id_fkey FOREIGN KEY (ride_booking_id) REFERENCES atlas_transporter.ride_booking(id)
);
ALTER TABLE atlas_transporter.notification_status OWNER TO atlas_transporter_user;
CREATE INDEX notification_status_driver_id_idx ON atlas_transporter.notification_status USING btree (driver_id);
CREATE INDEX notification_status_ride_id_idx ON atlas_transporter.notification_status USING btree (ride_booking_id);







CREATE TABLE atlas_transporter.allocation_event (
id character(36) NOT NULL,
ride_booking_id character(36) NOT NULL,
driver_id character(36),
event_type character varying(22) NOT NULL,
"timestamp" timestamp with time zone NOT NULL
,CONSTRAINT  allocation_event_pkey PRIMARY KEY (id)
,CONSTRAINT  allocation_event_ride_booking_id_fkey FOREIGN KEY (ride_booking_id) REFERENCES atlas_transporter.ride_booking(id)
);
ALTER TABLE atlas_transporter.allocation_event OWNER TO atlas_transporter_user;







CREATE TABLE atlas_transporter.ride (
id character(36) NOT NULL,
booking_id character(36) NOT NULL,
short_id character varying(36) NOT NULL,
status character varying(255) NOT NULL,
driver_id character(36) NOT NULL,
vehicle_id character(36) NOT NULL,
otp character(4) NOT NULL,
tracking_url character varying(255) NOT NULL,
fare double precision,
total_fare numeric(30,2),
traveled_distance double precision DEFAULT 0 NOT NULL,
chargeable_distance double precision,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  ride_pkey PRIMARY KEY (id)
,CONSTRAINT  ride_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES atlas_transporter.ride_booking(id)
,CONSTRAINT  ride_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_transporter.person(id)
,CONSTRAINT  ride_vehicle_id_fkey FOREIGN KEY (vehicle_id) REFERENCES atlas_transporter.vehicle(id)
);
ALTER TABLE atlas_transporter.ride OWNER TO atlas_transporter_user;

-- necessary data
INSERT INTO atlas_transporter.rider_details VALUES
	('UNKNOWN                             ', 'UNKNOWN', 'UNKNOWN', '\x', '2022-04-12 15:28:42.814589+00', '2022-04-12 15:28:42.814589+00');
INSERT INTO atlas_transporter.cancellation_reason VALUES
	('NOT_REACHABLE', 'Customer not reachable.', true),
	('VEHICLE_ISSUE', 'Vehicle issue.', true),
	('ACCEPTED_BY_MISSTAKE', 'Accepted by mistake.', true),
	('PICKUP_LOC_TOO_FAR', 'Pickup location was too far.', true),
	('COVID_RISK', 'COVID Risk. Customer not wearing mask/unwell.', true),
	('EXCESS_LUGGAGE', 'Excess luggage.', true),
	('TOO_MUCH_PASSANGERS', 'Passenger limit exceeded.', true),
	('OTHER', 'Some other reason.', true);

-- thank you for getting here!
