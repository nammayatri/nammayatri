-- auxiliary settings
CREATE USER atlas_public_transport_user WITH PASSWORD 'atlas';
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
CREATE SCHEMA atlas_public_transport;
ALTER SCHEMA atlas_public_transport OWNER TO atlas_public_transport_user;

-- tables






CREATE TABLE atlas_public_transport.search (
id character(36) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL,
requestor_id character(36) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  search_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_public_transport.search OWNER TO atlas_public_transport_user;







CREATE TABLE atlas_public_transport.transport_station (
id character(36) NOT NULL,
name character varying(255) NOT NULL,
station_code character varying(255) NOT NULL,
lat double precision NOT NULL,
lon double precision NOT NULL
,CONSTRAINT  transport_station_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_public_transport.transport_station OWNER TO atlas_public_transport_user;







CREATE TABLE atlas_public_transport.quote (
id character(36) NOT NULL,
search_id character(36) NOT NULL,
bpp_id character(36) NOT NULL,
bpp_url character varying(255) NOT NULL,
description character varying(255) NOT NULL,
fare numeric(30,2) NOT NULL,
departure_time timestamp with time zone,
arrival_time timestamp with time zone,
departure_station_id character(36) NOT NULL,
arrival_station_id character(36) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
route_code character varying(255) NOT NULL
,CONSTRAINT  quote_pkey PRIMARY KEY (id)
,CONSTRAINT  quote_arrival_station_id_fkey FOREIGN KEY (arrival_station_id) REFERENCES atlas_public_transport.transport_station(id)
,CONSTRAINT  quote_departure_station_id_fkey FOREIGN KEY (departure_station_id) REFERENCES atlas_public_transport.transport_station(id)
,CONSTRAINT  quote_search_id_fkey FOREIGN KEY (search_id) REFERENCES atlas_public_transport.search(id)
);
ALTER TABLE atlas_public_transport.quote OWNER TO atlas_public_transport_user;







CREATE TABLE atlas_public_transport.booking (
id character(36) NOT NULL,
search_id character(36) NOT NULL,
quote_id character(36) NOT NULL,
bkn_txn_id character(36) NOT NULL,
requestor_id character(36) NOT NULL,
quantity integer NOT NULL,
bpp_id character(36) NOT NULL,
bpp_url character varying(255) NOT NULL,
public_transport_support_number character varying(16) NOT NULL,
description character varying(255) NOT NULL,
fare numeric(30,2) NOT NULL,
departure_time timestamp with time zone,
arrival_time timestamp with time zone,
departure_station_id character(36) NOT NULL,
arrival_station_id character(36) NOT NULL,
status character varying(255) NOT NULL,
ticket_id character varying(255),
ticket_created_at timestamp with time zone,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  booking_pkey PRIMARY KEY (id)
,CONSTRAINT  booking_arrival_station_id_fkey FOREIGN KEY (arrival_station_id) REFERENCES atlas_public_transport.transport_station(id)
,CONSTRAINT  booking_departure_station_id_fkey FOREIGN KEY (departure_station_id) REFERENCES atlas_public_transport.transport_station(id)
,CONSTRAINT  booking_quote_id_fkey FOREIGN KEY (quote_id) REFERENCES atlas_public_transport.quote(id)
,CONSTRAINT  booking_search_id_fkey FOREIGN KEY (search_id) REFERENCES atlas_public_transport.search(id)
);
ALTER TABLE atlas_public_transport.booking OWNER TO atlas_public_transport_user;







CREATE TABLE atlas_public_transport.payment_transaction (
id character(36) NOT NULL,
booking_id character(36) NOT NULL,
bkn_txn_id character(36) NOT NULL,
payment_gateway_txn_id character varying(255) NOT NULL,
payment_gateway_txn_status character varying(255) NOT NULL,
fare numeric(30,2) NOT NULL,
status character varying(255) NOT NULL,
payment_url character varying(255) NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  payment_transaction_booking_id_key UNIQUE (booking_id)
,CONSTRAINT  payment_transaction_pkey PRIMARY KEY (id)
,CONSTRAINT  payment_transaction_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES atlas_public_transport.booking(id)
);
ALTER TABLE atlas_public_transport.payment_transaction OWNER TO atlas_public_transport_user;

-- necessary data

-- thank you for getting here!
