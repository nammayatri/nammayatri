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

ALTER SCHEMA atlas_public_transport OWNER TO atlas;

SET default_tablespace = '';

SET default_table_access_method = heap;

CREATE TABLE atlas_public_transport.transport_station (
    id character(36) NOT NULL PRIMARY KEY,
    name character varying(255) NOT NULL,
    station_code character varying(255) NOT NULL,
    lat double precision NOT NULL,
    lon double precision NOT NULL
);

CREATE TABLE atlas_public_transport.search (
    id character(36) NOT NULL PRIMARY KEY,
    lat double precision NOT NULL,
    lon double precision NOT NULL,
    requestor_id character(36) NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    from_date timestamp with time zone NOT NULL,
    to_date timestamp with time zone NOT NULL
);

CREATE TABLE atlas_public_transport.quote (
    id character(36) NOT NULL PRIMARY KEY,
    search_id character(36) NOT NULL REFERENCES atlas_public_transport.search (id),
    bpp_id character(36) NOT NULL,
    bpp_url character varying(255) NOT NULL,
    description character varying(255) NOT NULL,
    fare numeric(30,2) NOT NULL,
    departure_time timestamp with time zone,
    arrival_time timestamp with time zone,
    departure_station_id character(36) NOT NULL REFERENCES atlas_public_transport.transport_station (id),
    arrival_station_id character(36) NOT NULL REFERENCES atlas_public_transport.transport_station (id),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    route_code character varying(255) NOT NULL
);

CREATE TABLE atlas_public_transport.booking (
    id character(36) NOT NULL PRIMARY KEY,
    search_id character(36) NOT NULL REFERENCES atlas_public_transport.search (id),
    quote_id character(36) NOT NULL REFERENCES atlas_public_transport.quote (id),
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
    departure_station_id character(36) NOT NULL REFERENCES atlas_public_transport.transport_station (id),
    arrival_station_id character(36) NOT NULL REFERENCES atlas_public_transport.transport_station (id),
    status character varying(255) NOT NULL,
    ticket_id character varying(255),
    ticket_created_at timestamp with time zone,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_public_transport.payment_transaction (
    id character(36) NOT NULL PRIMARY KEY,
    booking_id character(36) NOT NULL REFERENCES atlas_public_transport.booking (id) UNIQUE,
    bkn_txn_id character(36) NOT NULL,
    payment_gateway_txn_id character varying(255) NOT NULL,
    payment_gateway_txn_status character varying(255) NOT NULL,
    fare numeric(30,2) NOT NULL,
    status character varying(255) NOT NULL,
    payment_url character varying(255) NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
