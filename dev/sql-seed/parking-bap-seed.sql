-- auxiliary settings
CREATE USER atlas_parking_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_parking;
ALTER SCHEMA atlas_parking OWNER TO atlas_parking_user;

-- tables






CREATE TABLE atlas_parking.parking_location (
id character(36) NOT NULL,
id_from_bpp character varying(255) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
name character varying(255) NOT NULL,
street_address character varying(255) NOT NULL,
locality character varying(255) NOT NULL,
city character varying(255),
state character varying(255) NOT NULL,
country character varying(255) NOT NULL,
area_code character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  parking_location_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_parking.parking_location OWNER TO atlas_parking_user;







CREATE TABLE atlas_parking.search (
id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
requestor_id character(36) NOT NULL,
from_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
to_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  search_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_parking.search OWNER TO atlas_parking_user;







CREATE TABLE atlas_parking.quote (
id character(36) NOT NULL,
search_id character(36) NOT NULL,
bpp_id character varying(255) NOT NULL,
bpp_url character varying(255) NOT NULL,
parking_space_name character varying(255) NOT NULL,
parking_location_id character(36) NOT NULL,
parking_location_id_from_bpp character varying(255) NOT NULL,
fare numeric(30,2) NOT NULL,
available_spaces integer NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
bpp_item_id character varying(255) NOT NULL
,CONSTRAINT  quote_pkey PRIMARY KEY (id)
,CONSTRAINT  quote_parking_location_id_fkey FOREIGN KEY (parking_location_id) REFERENCES atlas_parking.parking_location(id)
,CONSTRAINT  quote_search_id_fkey FOREIGN KEY (search_id) REFERENCES atlas_parking.search(id)
);
ALTER TABLE atlas_parking.quote OWNER TO atlas_parking_user;







CREATE TABLE atlas_parking.booking (
id character(36) NOT NULL,
search_id character(36) NOT NULL,
quote_id character(36) NOT NULL,
requestor_id character(36) NOT NULL,
requestor_number character varying(16) NOT NULL,
vehicle_number character varying(16) NOT NULL,
bpp_id character varying(255) NOT NULL,
bpp_url character varying(255) NOT NULL,
parking_space_name character varying(255) NOT NULL,
parking_space_location_id character(36) NOT NULL,
fare numeric(30,2) NOT NULL,
from_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
to_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
status character varying(255) NOT NULL,
ticket_id character varying(255),
ticket_created_at timestamp with time zone,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
bpp_item_id character varying(255) NOT NULL,
bpp_order_id character varying(255),
requestor_name character varying(255)
,CONSTRAINT  booking_pkey PRIMARY KEY (id)
,CONSTRAINT  booking_quote_id_fkey FOREIGN KEY (quote_id) REFERENCES atlas_parking.quote(id)
,CONSTRAINT  booking_search_id_fkey FOREIGN KEY (search_id) REFERENCES atlas_parking.search(id)
);
ALTER TABLE atlas_parking.booking OWNER TO atlas_parking_user;







CREATE TABLE atlas_parking.payment_transaction (
id character(36) NOT NULL,
booking_id character(36) NOT NULL,
payment_gateway_txn_id character varying(255) NOT NULL,
fare numeric(30,2) NOT NULL,
status character varying(255) NOT NULL,
payment_url character varying(255) NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
payment_gateway_txn_status character(36) NOT NULL
,CONSTRAINT  payment_transaction_booking_id_key UNIQUE (booking_id)
,CONSTRAINT  payment_transaction_pkey PRIMARY KEY (id)
,CONSTRAINT  payment_transaction_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES atlas_parking.booking(id)
);
ALTER TABLE atlas_parking.payment_transaction OWNER TO atlas_parking_user;

-- necessary data

-- thank you for getting here!
