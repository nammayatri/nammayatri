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

ALTER SCHEMA atlas_parking OWNER TO atlas;

SET default_tablespace = '';

SET default_table_access_method = heap;

CREATE OR REPLACE FUNCTION atlas_parking.uuid_generate_v4() RETURNS character (36)
    AS 'SELECT uuid_in(overlay(overlay(md5(random()::text 
        || '':'' || clock_timestamp()::text) placing ''4'' from 13) 
        placing to_hex(floor(random()*(11-8+1) + 8)::int)::text from 17)::cstring)::character (36);'
    LANGUAGE SQL
    IMMUTABLE;

CREATE TABLE atlas_parking.parking_search (
    id character(36) NOT NULL PRIMARY KEY DEFAULT atlas_parking.uuid_generate_v4(),
    search_location_id character(36) NOT NULL,
    requestor_id character(36) NOT NULL,
    from_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    to_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_parking.parking_quote (
    id character(36) NOT NULL PRIMARY KEY DEFAULT atlas_parking.uuid_generate_v4(),
    search_id character(36) NOT NULL REFERENCES atlas_parking.parking_search (id) on delete cascade,
    additional_info character varying(255) NOT NULL,
    bpp_id character(36) NOT NULL,
    bpp_url character varying(255) NOT NULL,
    parking_space_name character varying(255) NOT NULL,
    parking_space_location_id character(36) NOT NULL,
    parking_support_number character varying(16) NOT NULL,
    fare numeric(30,2) NOT NULL,
    available_spaces integer NOT NULL,
    max_spaces integer NOT NULL,
    from_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    to_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_parking.parking_booking (
    id character(36) NOT NULL PRIMARY KEY DEFAULT atlas_parking.uuid_generate_v4(),
    search_id character(36) NOT NULL REFERENCES atlas_parking.parking_search (id) on delete cascade,
    quote_id character(36) NOT NULL REFERENCES atlas_parking.parking_quote (id) on delete cascade,
    requestor_id character(36) NOT NULL,
    requestor_number character varying(16) NOT NULL,
    vehicle_number character varying(16) NOT NULL,
    additional_info character varying(255) NOT NULL,
    bpp_id character(36) NOT NULL,
    bpp_url character varying(255) NOT NULL,
    parking_space_name character varying(255) NOT NULL,
    parking_space_location_id character(36) NOT NULL,
    parking_support_number character varying(16) NOT NULL,
    fare numeric(30,2) NOT NULL,
    from_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    to_date timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    status character varying(255) NOT NULL,
    ticket_id character varying(255),
    ticket_created_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_parking.payment_transaction (
    id character(36) NOT NULL PRIMARY KEY DEFAULT atlas_parking.uuid_generate_v4(),
    booking_id character(36) NOT NULL REFERENCES atlas_parking.parking_booking (id) on delete cascade,
    bkn_txn_id character(36) NOT NULL,
    payment_gateway_txn_id character varying(255) NOT NULL,
    fare numeric(30,2) NOT NULL,
    status character varying(255) NOT NULL,
    payment_url character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);